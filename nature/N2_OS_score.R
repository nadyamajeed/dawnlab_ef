library(dplyr)

#####

score_nature2 <- function(
  rawdata, anskey, serial = TRUE,
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
  recall.scores <- data.frame(participantID = d.out$ResponseId)
  
  # score recall trials
  for(qn in recall.qns){
    
    # extract answers for this set
    answers <- strsplit(as.character(recall.ansKey[1, qn]),',',fixed=TRUE)
    answers <- answers[[1]] %>% trimws()
    if(show_progress | debug){cat("Set size is:", length(answers), "\n"); cat("Answers are:"); print(answers)}
    
    # extract participants' responses for this set
    responses <- recall.data[, qn]
    scores <- vector()
    # look at each participant
    for(response in responses){
      # extract
      words <- strsplit(response,',',fixed=TRUE); words <- words[[1]] %>% trimws()
      # score each response for this participant (SERIAL)
      if(serial){
        current_score <- 0
        for(i in 1:length(answers)){if(i <= length(words)){if(words[i] == answers[i]){current_score <- current_score + 1}}}
        current_score <- current_score / length(answers)
      }
      # score each response for this participant (NON-SERIAL)
      if(!serial){
        words <- unique(sort(words))
        current_score <- sum(words %in% answers) / length(answers)
      }
      # record this participant's score
      scores <- c(scores, current_score)
    }
    
    # record all participants' scores for this set
    recall.scores[, qn] <- scores
  } 
  
  # calc overall score for each participant and add
  d.out$recallScore <- recall.scores %>% dplyr::select(-participantID) %>% rowSums(na.rm = T)
  
  # add each recall set score
  d.out <- cbind(d.out, recall.scores)
  
  # print Cronbach's alpha
  if(cronbach) {
    if("psych" %in% rownames(installed.packages()) == FALSE) {install.packages("psych")}
    psych::alpha(recall.scores, check.keys = FALSE) %>% print()
  }
  
  #####
  
  # export
  prefix = ifelse(serial, "scoredSerial_", "scoredNonserial_")
  if(is_csv) {write.csv(d.out, paste0(prefix, rawdata), row.names = F)}
  if(is_sav) {haven::write_sav(d.out %>% haven::zap_label() %>% haven::zap_labels(), paste0(prefix, rawdata))}
  
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
