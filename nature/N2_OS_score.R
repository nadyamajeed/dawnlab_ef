library(dplyr)

#####

score_nature2 <- function(
  rawdata, anskey,
  distractor_pattern = "DIST", recall_pattern = "RECALLSET",
  cronbach = TRUE, show_progress = TRUE, debug = FALSE) {
  
  starttime <- Sys.time()
  
  # check if csv, import
  is_csv <- grepl(".csv", rawdata)
  if(is_csv) {d <- read.csv(rawdata)}
  
  # check if sav, import
  is_sav <- grepl(".sav", rawdata)
  if(is_sav) {d <- haven::read_sav(rawdata)}
  
  # if not csv or sav, stop function
  if(!is_csv & !is_sav) stop("rawdata should be in .csv or .sav format!")
  
  # if debug, show
  if(debug) {print(head(d))}
  
  # compute distractor acc and match to participant IDs
  d.out <- d %>% dplyr::select(contains(distractor_pattern))
  d.out$distractorAcc <- rowMeans(d.out, na.rm = TRUE)
  d.out$distractorAcc[is.nan(d.out$distractorAcc)] <- NA
  d.out$ResponseId <- d$ResponseId
  d.out <- d.out %>% dplyr::select(ResponseId, distractorAcc)
  if(show_progress | debug){head(d.out) %>% print()}
  
  # view distractorAcc distribution
  p <- ggplot(data = d.out, aes(x = distractorAcc)) +
    geom_density() + geom_vline(xintercept = 0.65, color = "red") +
    theme_classic()
  print(p)
  
  #####
  
  # load set size and ans key
  recall.ansKey <- read.csv(anskey, colClasses = "character")
  if(show_progress | debug){head(recall.ansKey) %>% print()}
  
  # load recall data
  recall.data <- d %>%
    dplyr::select(contains(recall_pattern)) %>%
    dplyr::mutate_all(.funs = trimws)
  if(show_progress | debug){head(recall.data) %>% print()}
  
  # prepare for scoring
  recall.qns <- colnames(recall.ansKey)
  recall.scores <- data.frame(
    participantID = d.out$ResponseId,
    RECALL4A = NA,
    RECALL4B = NA,
    RECALL5A = NA,
    RECALL5B = NA,
    RECALL6A = NA,
    RECALL6B = NA
  ) %>%
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
  
  # calc overall score for each participant and add
  d.out$recallScore <- recall.scores %>% rowSums(na.rm = T)
  
  # add each recall set score
  d.out <- cbind(d.out, recall.scores)
  
  # print Cronbach's alpha
  if(cronbach) {
    if("psych" %in% rownames(installed.packages()) == FALSE) {install.packages("psych")}
    psych::alpha(recall.scores, check.keys = FALSE) %>% print()
  }
  
  #####
  
  # export
  if(is_csv) {write.csv(d.out, paste0("scored", rawdata), row.names = F)}
  if(is_sav) {haven::write_sav(d.out %>% haven::zap_label() %>% haven::zap_labels(), paste0("scored", rawdata))}
  
  # visual inspection
  library(ggplot2)
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
