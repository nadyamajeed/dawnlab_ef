library(devtools)
source_url("https://raw.githubusercontent.com/nadyaeiou/nadyasscripts/main/all.R")
library(ggplot2)

#####

score_stereo_us <- function(rawdata, show_progress = FALSE, distractorScore = "os_m.score", cronbach = FALSE) {
  
  starttime <- Sys.time()
  
  # check if csv, import
  is_csv <- grepl(".csv", rawdata)
  if(is_csv) {d <- read.csv(rawdata)}
  
  # check if sav, import
  is_sav <- grepl(".sav", rawdata)
  if(is_sav) {library(haven); d <- read_sav(rawdata)}
  
  # if not csv or sav, stop function
  if(!is_csv & !is.sav) stop("rawdata should be in .csv or .sav format!")
  
  # load participant IDs and get distractor acc
  d.out <- d[ , c("ResponseId", distractorScore)] %>%
  d.out$distractorAcc <- d.out[ , distractorScore] / 50
  if(show_progress){head(d.out) %>% print()}
  
  # view distractorAcc distribution
  p <- ggplot(data = d.out, aes(x = distractorAcc)) +
    geom_density() + geom_vline(xintercept = 0.65, color = "red") +
    theme_classic()
  print(p)
  
  #####
  
  # load set size and ans key
  recall.ansKey <- read.csv("https://raw.githubusercontent.com/nadyaeiou/dawnlab/main/stereo/us/answerkey.csv", colClasses = "character")
  if(show_progress){head(recall.ansKey) %>% print()}
  
  # load recall data
  recall.data <- d %>%
    dplyr::select(contains("OS_W")) %>%
    dplyr::select(-contains("score")) %>%
    dplyr::mutate_all(.funs = toupper) %>%
    dplyr::mutate_all(.funs = trimws)
  if(show_progress){head(recall.data) %>% print()}
  
  # prepare for scoring
  recall.qns <- colnames(recall.ansKey)
  recall.scores <- data.frame(
    participantID = d.out$ResponseId,
    W3a = NA,
    W3b = NA,
    W4a = NA,
    W4b = NA,
    W5a = NA,
    W5b = NA,
    W6a = NA,
    W6b = NA,
    W7a = NA,
    W7b = NA) %>%
    dplyr::select(-participantID)
  recall.sets <- colnames(recall.scores)
  
  # score recall trials
  recall.data.scored <- recall.data
  for(qn in recall.qns){
    
    answer <- recall.ansKey[1, qn]
    
    if(show_progress){
      cat("\nNow scoring:", qn)
      cat("\nAnswer is:", answer, "\n")
    }
    
    recall.data.scored[, qn] <- ifelse(recall.data.scored[, qn] == answer, 1, 0)
  }; rm(qn); rm(answer)
  
  # score recall sets
  for(setname in recall.sets){
    currentset <- recall.data.scored %>% dplyr::select(contains(setname))
    if(show_progress){
      cat("\nNow calculating set PCU:", setname, "\n")
      head(currentset) %>% print()
      }
    recall.scores[, setname] <- rowMeans(currentset)
  }; rm(setname); rm(currentset)
  
  # calc acc for each participant
  d.out$recallScore <- recall.scores %>% rowSums(na.rm = T)
  
  # print Cronbach's alpha
  if(cronbach) {
    if("psych" %in% rownames(installed.packages()) == FALSE) {install.packages("psych")}
    library(psych)
    psych::alpha(recall.scores, check.keys = FALSE) %>% print()
    }
  
  #####
  
  # export
  write.csv(d.out, paste0("scored", rawdata), row.names = F)
  
  # visual inspection
  p <- ggplot(data = d.out, aes(x = distractorAcc, y = recallScore)) +
    geom_point(size = 0.25) +
    geom_vline(xintercept = 0.65, color = "red") +
    ylim(c(0,10)) + xlim(c(0,1)) +
    theme_classic()
  print(p)
  
  endtime <- Sys.time()
  cat("\n##########\nDone scoring! Time taken:", endtime - starttime, "\n##########\n")
  return(d.out)
}
