library(devtools)
source_url("https://raw.githubusercontent.com/nadyaeiou/nadyasscripts/main/all.R")

#####

# load participant IDs
pID <- read.csv(data_file_name) %>% dplyr::select(Random.ID)
head(pID)

#####

# load distractor data
distractor.data <- read.csv(data_file_name) %>% dplyr::select(Random.ID, contains("_MATH"))
head(distractor.data)

# score accuracy for distractors
distractor.acc <- distractor.data %>%
  dplyr::select(-Random.ID) %>%
  rowMeans(na.rm = T)
head(distractor.acc)

#####

# load set size and ans key
recall.ansKey <- read.csv(anskey_file_name, row.names = 1) %>%
  mutate_if(is.factor, as.character) %>%
  dplyr::mutate(
    RECALL01 = tolower(OST4_RECALL1),
    RECALL02 = tolower(OST4_RECALL2),
    RECALL03 = tolower(OST5_RECALL1),
    RECALL04 = tolower(OST5_RECALL2),
    RECALL05 = tolower(OST6_RECALL1),
    RECALL06 = tolower(OST6_RECALL2),
    .keep = "unused"
  )
rownames(recall.ansKey) <- c("answerKey", "setSize")
head(recall.ansKey)

# load recall data
recall.data <- read.csv(data_file_name) %>%
  dplyr::select(Random.ID, contains("_RECALL")) %>%
  mutate_if(is.factor, as.character) %>%
  dplyr::mutate(
    RECALL01 = tolower(OST4_RECALL1),
    RECALL02 = tolower(OST4_RECALL2),
    RECALL03 = tolower(OST5_RECALL1),
    RECALL04 = tolower(OST5_RECALL2),
    RECALL05 = tolower(OST6_RECALL1),
    RECALL06 = tolower(OST6_RECALL2),
    .keep = "unused"
    )
head(recall.data)

# prepare for scoring
recall.qns <- colnames(recall.ansKey)
recall.scores <- data.frame(participantID = pID,
                            RECALL01 = NA,
                            RECALL02 = NA,
                            RECALL03 = NA,
                            RECALL04 = NA,
                            RECALL05 = NA,
                            RECALL06 = NA)
head(recall.scores)

# score recall data
for(qn in recall.qns){
  cat("\nNow scoring:", qn, "\n")

  answers <- strsplit(as.character(recall.ansKey["answerKey", qn]),', ',fixed=TRUE)
  answers <- answers[[1]]; answers <- sort(answers)
  cat("Set size is:", length(answers), "\n")
  cat("Answers are:")
  print(answers)

  responses <- recall.data[, qn]
  scores <- vector()
  for(response in responses){
    words <- strsplit(response,', ',fixed=TRUE); words <- words[[1]]; words <- unique(sort(words))
    current_score <- sum(words %in% answers) / length(answers)
    scores <- c(scores, current_score)
    rm(words); rm(current_score)
  }; rm(responses); rm(response)

  cat("Average acc for this recall is:", mean(scores), "\n")
  recall.scores[, qn] <- scores
  rm(scores)

}; rm(qn); rm(answers)
head(recall.scores)

# calc acc for each participant
recall.acc <- recall.scores %>%
  dplyr::select(-Random.ID) %>%
  rowSums(na.rm = T)
head(recall.acc)

#####

# prepare export

export <- data.frame(
  participantID = pID,
  distractorAcc = distractor.acc,
  recallScore = recall.acc)

write.csv(export, paste0("scored_nonserial_", data_file_name), row.names = F)
