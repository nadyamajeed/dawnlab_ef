setwd("~/Desktop/Research/EFDIARY/tatool/reprogaminhib")
library(dplyr)

# function to rewrite trials
rewrite_trials = function(originalcsv) {
  # retrieve possible whole rows for congruent trials, and for incongruent trials
  df.fullrows.congruent = originalcsv %>% dplyr::filter(stimulusType == "congruent") %>% distinct()
  df.fullrows.incongruent = originalcsv %>% dplyr::filter(stimulusType == "incongruent") %>% distinct()

  # function to create trials
  create_trials = function(ntrials, possiblerows) {
    created_trials = possiblerows[sample(nrow(possiblerows), ntrials, replace = TRUE), ]
    rownames(created_trials) = NULL
    return(created_trials)
  }

  # create 100 congruent trials
  df.filled.congruent = create_trials(100, df.fullrows.congruent)

  # create 100 incongruent trials
  df.filled.incongruent = create_trials(100, df.fullrows.incongruent)

  # bind together and shuffle order
  df.filled = rbind(df.filled.congruent, df.filled.incongruent)
  df.filled = df.filled[sample(nrow(df.filled), 200, replace = FALSE), ]
  rownames(df.filled) = NULL

  # check how many cong and incong
  nrow(df.filled[df.filled$stimulusType == "congruent", ]) %>% print()
  nrow(df.filled[df.filled$stimulusType == "incongruent", ]) %>% print()

  # return written trials
  return(df.filled)
}

set.seed(0)

##### load and rewrite flanker data
f.raw = read.csv("ef_inhibition_flanker.csv", sep = ";") %>% dplyr::mutate_all(as.character)
f.new = rewrite_trials(f.raw)
write.table(f.new, "ef_inhibition_flanker_new.csv", row.names = FALSE, quote = FALSE, sep = ";")

##### load and rewrite flanker data
s.raw = read.csv("ef_inhibition_stroop.csv", sep = ";") %>% dplyr::mutate_all(as.character)
s.new = rewrite_trials(s.raw)
write.table(s.new, "ef_inhibition_stroop_new.csv", row.names = FALSE, quote = FALSE, sep = ";")
