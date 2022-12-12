library(dplyr)
library(tidyverse)

############################## CLEAN TATOOL DATA ##############################

cleanTatoolGeneral = function(task.importlabel, path1, path2, debug = FALSE) {

  task.exportlabel = case_when(
    task.importlabel == "color"     ~ "u1",
    task.importlabel == "letter"    ~ "u2",
    task.importlabel == "number"    ~ "u3",
    task.importlabel == "simon"     ~ "ic1",
    task.importlabel == "flanker"   ~ "ic2",
    task.importlabel == "stroop"    ~ "ic3",
    task.importlabel == "category"  ~ "ts1",
    task.importlabel == "figural"   ~ "ts2",
    task.importlabel == "numerical" ~ "ts3"
  )

  ##### read tatool data -----

  wd = getwd() # get wd so we can restore it later

  setwd(path1)
  tatool1 = list.files(pattern = "*.csv") %>% map_df(~read_csv(.)) %>%
    dplyr::mutate(
      session_format = "Combined tasks",
      session.forceExit = NULL,
      stimulusPosition = NULL,
      stimulusValue = NULL
    )
  if(debug) View(tatool1)

  setwd(wd); setwd(path2)
  tatool2 = list.files(pattern = "*.csv") %>% map_df(~read_csv(.)) %>%
    dplyr::mutate(
      session_format = "Indiv task",
      session.forceExit = NULL,
      stimulusPosition = NULL,
      stimulusValue = NULL
    )
  if(debug) View(tatool2)

  if(task.importlabel %in% c("color", "letter", "number")) {
    tatool1 = tatool1 %>% dplyr::mutate(reactionTime = NULL)
    tatool2 = tatool2 %>% dplyr::mutate(reactionTime = NULL)
  }

  setwd(wd) # reset wd to original

  ##### throw out some columns and rows, do some basic cleaning -----

  tatool = rbind(tatool1, tatool2) %>%
    dplyr::mutate(
      participantID = trimws(toupper(extId)),
      participantID = case_when(
        # fix one participant who didn't key in participantID
        sessionToken == "lBDokTTr1628496223162" ~ "WJH12B81BLACK",
        # leave the rest of the participantIDs untouched
        TRUE                                    ~ participantID
      ),
      sessionToken,
      tasknum = task.exportlabel,
      taskname = task.importlabel,
      .keep = "unused"
    ) %>%
    dplyr::filter(
      !is.na(participantID) & participantID != ""
      & participantID != "TESTNADYA" & participantID != "TESTNADYA2"
      & participantID != "TESTNADYA3" & participantID != "TESTGLORIA"
      & participantID != "TESTXINYI" & participantID != "TESTJOAX"
      & participantID != "CLSC12B56PINK" & participantID != "CPCWINK15"
      & participantID != "KB17S65PURPLE" & participantID != "LEE07R76BLUE"
      & participantID != "SB07LAVENDER00BLUE" & participantID != "TTP04P98BLUE"
      & userCode != "138051" & userCode != "138050"
    ) %>%
    dplyr::select(-session.condition, -userCode, -moduleId)

  if(debug) View(tatool)

  rm(tatool1); rm(tatool2)

  ##### task-specific cleaning -----

  # cleaning specific to updating tasks
  if(task.importlabel %in% c("color", "letter", "number")) {
    tatool = tatool %>%
      # remove practice trials and only retain trials for this specific task
      dplyr::filter(
        !str_detect(executableId, "practice")
        & str_detect(executableId, task.importlabel)
      ) %>%
      # remove extra trials for two participants who have extra somehow
      dplyr::filter(sessionToken != "sOhEILQ01627024346755" & sessionToken != "haXloZHX1626696095953")
  }

  # cleaning specific to ic tasks
  if(task.importlabel %in% c("simon", "flanker", "stroop")) {
    tatool = tatool %>%
      dplyr::filter(
        !grepl("practice", executableId) & grepl(task.importlabel, executableId)
      ) %>%
      dplyr::filter(participantID != "YCL5H43BROWN")
  }

  # cleaning specific to ts tasks
  if(task.importlabel %in% c("category", "figural", "numerical")) {
    tatool = tatool %>%
      dplyr::filter(
        !grepl("practice", executableId) & grepl(task.importlabel, executableId)
        & shiftingType != "start"
      ) %>%
      dplyr::filter(
        participantID != "SW19N35PINK" & participantID != "TBC01TAMPINES EASY16PURPLE"
      )
    if(task.importlabel == "category") {
      tatool[tatool$executableId == "category_single_animacy_1", "shiftingType"] = "single"
      tatool[tatool$executableId == "category_single_airyness_1", "shiftingType"] = "single"
      tatool[tatool$executableId == "category_single_airyness_2", "shiftingType"] = "single"
      tatool[tatool$executableId == "category_single_animacy_2", "shiftingType"] = "single"
    }
  }

  ##### get counts -----

  # how many participants
  cat("\nNo. of distinct participants:", nrow(distinct(tatool, participantID)))

  # how many sessions
  cat("\nNo. of distinct sessions    :", nrow(distinct(tatool, sessionToken)), "\n")

  ##### drop incompletes -----

  # get trialId_gap for each participant
  tatool = tatool %>%
    group_by(sessionToken) %>%
    dplyr::mutate(
      trialId_min = min(trialId),
      trialId_max = max(trialId),
      trialId_gap = trialId_max - trialId_min
    ) %>%
    ungroup() %>%
    dplyr::select(sessionToken, trialId, trialId_gap, everything()) %>%
    dplyr::select(-trialId_min, -trialId_max)

  # check if anyone didn't complete
  correct_gap = case_when(
    task.importlabel == "color"     ~ 124,
    task.importlabel == "letter"    ~ 124,
    task.importlabel == "number"    ~ 99,
    task.importlabel == "simon"     ~ (212 - 13),
    task.importlabel == "flanker"   ~ (212 - 13),
    task.importlabel == "stroop"    ~ (212 - 13),
    task.importlabel == "category"  ~ 407,
    task.importlabel == "figural"   ~ 408,
    task.importlabel == "numerical" ~ 408
  )

  # report who is dropped
  whoDrop = tatool[tatool$trialId_gap != correct_gap, c("participantID", "sessionToken")] %>% distinct()
  cat("\nDropping (no. of incomplete sessions):", whoDrop %>% nrow(), "\n")
  if(debug) {merge(whoDrop, tatool, all.x = TRUE) %>% View()}

  # drop
  tatool = tatool %>% dplyr::filter(trialId_gap == correct_gap) %>% dplyr::select(-trialId_gap)

  # report number of complete sessions remaining
  cat("\nNo. of complete sessions remaining    :", nrow(distinct(tatool, sessionToken)))
  cat("\nNo. of distinct participants remaining:", nrow(distinct(tatool, participantID)), "\n")

  ##### calculate global acc for each participant and drop < .50 (IC & TS only) -----

  if(task.importlabel %in% c("simon", "flanker", "stroop", "category", "figural", "numerical")) {
    tatool = tatool %>%
      group_by(sessionToken) %>%
      mutate(session_acc_global = mean(score)) %>%
      ungroup()
    cat("\nGLOBAL ACCURACY BEFORE DROPPING POOR ACC PEOPLE?",
        "\nGlobal Acc: M =", round(mean(tatool$session_acc_global), 2) * 100,
        "%, SD =", round(sd(tatool$session_acc_global), 2) * 100,
        "%, min =", round(min(tatool$session_acc_global), 2) * 100,
        "%\n")
    cat("\nDROPPING POOR ACC PEOPLE NOW...")
    tatool = tatool %>% dplyr::filter(session_acc_global >= .50)
    cat("\nNo. of distinct participants remaining:", nrow(distinct(tatool, participantID)))
  }

  ##### return cleaned tatool data -----
  tatool = tatool %>% dplyr::select(participantID, tasknum, taskname, everything())
  return(tatool)

}



############################## SCORE CLEANED DATA ##############################

scoreTatoolU = function(cleaned_data, write = FALSE, debug = FALSE) {

  tatool = cleaned_data
  task.exportlabel = cleaned_data[1, "tasknum"]

  ##### calculate updating acc (i.e., for nSteps not 0) -----
  tatool = tatool %>%
    group_by(sessionToken) %>%
    dplyr::filter(nSteps > 0) %>%
    dplyr::rename(trial_score = score) %>%
    mutate(session_score = mean(trial_score, na.rm=T)) %>%
    ungroup()

  ##### clean trial- and session-level dfs and return -----
  tatool.trial = tatool %>%
    dplyr::arrange(participantID, trialId) %>%
    dplyr::select(participantID, sessionToken, tasknum, taskname, trialId, everything())
  tatool.session = tatool.trial %>%
    distinct(sessionToken, .keep_all = T) %>%
    dplyr::select(
      participantID, sessionToken, tasknum, taskname,
      contains("session")
    )

  ##### write & return the two dfs -----
  if(write){
    write.csv(tatool.trial, paste0("efScored_", task.exportlabel, "_triallevel.csv"))
    write.csv(tatool.session, paste0("efScored_", task.exportlabel, "_sessionlevel.csv"))
  }
  return(list(trial = tatool.trial, session = tatool.session))
}

scoreTatoolIC = function(cleaned_data, sdcutoff = 3, write = FALSE, debug = FALSE) {

  tatool = cleaned_data
  task.exportlabel = cleaned_data[1, "tasknum"]
  task.importlabel = cleaned_data[1, "taskname"]

  ##### calculate acc (NO TRIMMING FOR ACC)

  # acc by type
  scores_acc = aggregate(
    tatool$score,
    by = list(tatool$sessionToken, tatool$stimulusType),
    FUN = mean)
  colnames(scores_acc) = c("sessionToken", "stimulusType", "acc")

  # reformat acc into wide format
  scores_acc = scores_acc %>%
    tidyr::pivot_wider(
      id_cols = sessionToken,
      names_prefix = "session_acc_",
      names_from = stimulusType,
      values_from = acc
    ) %>%
    dplyr::mutate(
      # interference effect = incongruent - congruent
      session_acc_interference = session_acc_incongruent - session_acc_congruent,
    )

  # if not simon task, calc better interference effect also
  if(task.importlabel != "simon") {
    scores_acc = scores_acc %>%
      dplyr::mutate(
        # better interference effect = incongruent - neutral
        session_acc_betterinterference = session_acc_incongruent - session_acc_neutral
      )
  }

  # merge back into master
  tatool = merge(tatool, scores_acc, all = T); rm(scores_acc)



  ##### calculate rt (WITH TRIMMING) -----

  ## cleaning first ##

  # delete wrong trials
  tatool.rt = tatool %>% dplyr::filter(score == 1)

  # delete rt < 200 (trimming step)
  tatool.rt = tatool.rt %>% dplyr::filter(reactionTime >= 200)

  # create Z-scored RT within each participant
  tatool.rt = tatool.rt %>%
    group_by(sessionToken) %>%
    mutate(trial_rt_z = scale(reactionTime)) %>%
    ungroup()

  # keep those within sd range (trimming step)
  tatool.rt = tatool.rt %>% dplyr::filter(trial_rt_z > -sdcutoff & trial_rt_z < sdcutoff)

  hist(tatool.rt$trial_rt_z)

  ## calculations ##

  # rt by type
  scores_rt = aggregate(
    tatool.rt$reactionTime,
    by = list(tatool.rt$sessionToken, tatool.rt$stimulusType),
    FUN = mean)
  colnames(scores_rt) = c("sessionToken", "stimulusType", "rt")

  # reformat rt into wide format
  scores_rt = scores_rt %>%
    tidyr::pivot_wider(
      id_cols = sessionToken,
      names_prefix = "session_rt_",
      names_from = stimulusType,
      values_from = rt
    ) %>%
    dplyr::mutate(
      # interference effect = incongruent - congruent
      session_rt_interference = session_rt_incongruent - session_rt_congruent,
    )

  # if not simon task, calc better interference effect also
  if(task.importlabel != "simon") {
    scores_rt = scores_rt %>%
      dplyr::mutate(
        # better interference effect = incongruent - neutral
        session_rt_betterinterference = session_rt_incongruent - session_rt_neutral
      )
  }

  # merge back into master
  tatool = merge(tatool, tatool.rt, all = T) %>% merge(scores_rt, all = T); rm(tatool.rt); rm(scores_rt)



  ##### bin for interference -----

  # trim <200ms (all trials) or +-sdcutoff (throw accurate trials only, keep inaccurate +-sdcutoff as long as >=200ms)

  tatool.trimmed = tatool %>%
    dplyr::filter(
      reactionTime >= 200 &
        ((trial_rt_z > -sdcutoff & trial_rt_z < sdcutoff) | is.na(trial_rt_z))
    )

  # binning is only for incongruent trials so only keep incongruent trials

  tatool.trimmed.incongruent = tatool.trimmed %>% dplyr::filter(stimulusType == "incongruent")
  rm(tatool.trimmed)

  # each participant’s “baseline” reaction time was first determined by taking the mean of their reaction times on accurate congruent trials

  # has alr been calculated earlier: tatool.trimmed.incongruent$session_rt_congruent

  # Then, for each participant, this mean was subtracted from their accurate incongruent trials, such that the new score for each accurate incongruent trial, termed the interference effect, reflected how fast or slow the participant reacted compared to their own baseline.

  tatool.trimmed.incongruent.accurate = tatool.trimmed.incongruent %>%
    dplyr::filter(score == 1) %>%
    dplyr::mutate(trial_rt_interference = reactionTime - session_rt_congruent)

  # Afterwards, all interference effects across all participants were rank ordered, and based on deciles, were assigned a bin value from 1 (fastest 10%) to 10 (slowest 10%)

  tatool.trimmed.incongruent.accurate = tatool.trimmed.incongruent.accurate %>%
    dplyr::mutate(trial_bin_interference = ntile(trial_rt_interference, 10))

  tatool.trimmed.incongruent = merge(tatool.trimmed.incongruent, tatool.trimmed.incongruent.accurate, all = T)
  rm(tatool.trimmed.incongruent.accurate)

  # Next, as a penalty for inaccuracy, all inaccurate incongruent trials were assigned a bin value of 20 regardless of actual reaction time

  tatool.trimmed.incongruent[tatool.trimmed.incongruent$score == 0, "trial_bin_interference"] = 20

  # Lastly, a mean bin score was computed for each participant.

  tatool = merge(tatool, tatool.trimmed.incongruent, all = T) %>%
    group_by(sessionToken) %>%
    mutate(session_bin_interference = mean(trial_bin_interference, na.rm = T)) %>%
    ungroup()
  rm(tatool.trimmed.incongruent)



  ##### bin for better interference -----
  # ONLY FOR NON-SIMON

  # if not simon task, calc better interference effect also
  if(task.importlabel != "simon") {

    # trim <200ms (all trials) or +-sdcutoff (throw accurate trials only, keep inaccurate +-sdcutoff as long as >=200ms)

    tatool.trimmed = tatool %>%
      dplyr::filter(
        reactionTime >= 200 &
          ((trial_rt_z > -sdcutoff & trial_rt_z < sdcutoff) | is.na(trial_rt_z))
      )

    # binning is only for incongruent trials so only keep incongruent trials

    tatool.trimmed.incongruent = tatool.trimmed %>% dplyr::filter(stimulusType == "incongruent")
    rm(tatool.trimmed)

    # each participant’s “baseline” reaction time was first determined by taking the mean of their reaction times on accurate neutral trials

    # has alr been calculated earlier: tatool.trimmed.incongruent$session_rt_neutral

    # Then, for each participant, this mean was subtracted from their accurate incongruent trials, such that the new score for each accurate incongruent trial, termed the better interference effect, reflected how fast or slow the participant reacted compared to their own baseline.

    tatool.trimmed.incongruent.accurate = tatool.trimmed.incongruent %>%
      dplyr::filter(score == 1) %>%
      dplyr::mutate(trial_rt_betterinterference = reactionTime - session_rt_neutral)

    # Afterwards, all better interference effects across all participants were rank ordered, and based on deciles, were assigned a bin value from 1 (fastest 10%) to 10 (slowest 10%)

    tatool.trimmed.incongruent.accurate = tatool.trimmed.incongruent.accurate %>%
      dplyr::mutate(trial_bin_betterinterference = ntile(trial_rt_betterinterference, 10))

    tatool.trimmed.incongruent = merge(tatool.trimmed.incongruent, tatool.trimmed.incongruent.accurate, all = T)
    rm(tatool.trimmed.incongruent.accurate)

    # Next, as a penalty for inaccuracy, all inaccurate incongruent trials were assigned a bin value of 20 regardless of actual reaction time

    tatool.trimmed.incongruent[tatool.trimmed.incongruent$score == 0, "trial_bin_betterinterference"] = 20

    # Lastly, a mean bin score was computed for each participant.

    tatool = merge(tatool, tatool.trimmed.incongruent, all = T) %>%
      group_by(sessionToken) %>%
      mutate(session_bin_betterinterference = mean(trial_bin_betterinterference, na.rm = T)) %>%
      ungroup()
    rm(tatool.trimmed.incongruent)

  }



  ##### clean trial- and session-level dfs and return -----
  tatool = tatool[order(tatool$participantID, tatool$trialId), ]
  tatool = tatool %>% dplyr::select(participantID, sessionToken, session_format, tasknum, taskname, trialId, everything())
  tatool.session = tatool %>%
    distinct(sessionToken, .keep_all = T) %>%
    dplyr::select(
      participantID, sessionToken, session_format, tasknum, taskname,
      contains("session")
    ) %>% dplyr::select(-sessionToken)



  ##### write+return the two dfs -----
  if(write){
    write.csv(tatool, paste0("efScored_", task.exportlabel, "_triallevel.csv"))
    write.csv(tatool.session, paste0("efScored_", task.exportlabel, "_sessionlevel.csv"))
  }
  return(list(trial = tatool, session = tatool.session))

}

scoreTatoolTS = function(cleaned_data, sdcutoff = 2.5, write = FALSE, debug = FALSE) {

  tatool = cleaned_data
  task.exportlabel = cleaned_data[1, "tasknum"]

  ##### calculate acc (NO TRIMMING FOR ACC)

  # acc by type
  scores_acc = aggregate(
    tatool$score,
    by = list(tatool$sessionToken, tatool$shiftingType),
    FUN = mean)
  colnames(scores_acc) = c("sessionToken", "shiftingType", "acc")

  # reformat acc into wide format
  scores_acc = scores_acc %>%
    tidyr::pivot_wider(
      id_cols = sessionToken,
      names_prefix = "session_acc_",
      names_from = shiftingType,
      values_from = acc
    ) %>%
    # calc switch cost and mix cost
    dplyr::mutate(
      # switch cost = switch - repetition
      session_acc_switchcost = session_acc_switch - session_acc_repetition,
      # mix cost = repetition - single
      session_acc_mixcost = session_acc_repetition - session_acc_single
    )

  # merge back into master
  tatool = merge(tatool, scores_acc, all = T); rm(scores_acc)




  ##### calculate rt (WITH TRIMMING) -----

  ## cleaning first ##

  # delete wrong trials
  tatool.rt = tatool %>% dplyr::filter(score == 1)

  # delete rt < 200 (trimming step)
  tatool.rt = tatool.rt %>% dplyr::filter(reactionTime >= 200)

  # create Z-scored RT within each participant
  tatool.rt = tatool.rt %>%
    group_by(sessionToken) %>%
    mutate(trial_rt_z = scale(reactionTime)) %>%
    ungroup()

  # keep those within sd range (trimming step)
  tatool.rt = tatool.rt %>% dplyr::filter(trial_rt_z > -sdcutoff & trial_rt_z < sdcutoff)

  hist(tatool.rt$trial_rt_z)

  ## calculations ##

  # rt by type
  scores_rt = aggregate(
    tatool.rt$reactionTime,
    by = list(tatool.rt$sessionToken, tatool.rt$shiftingType),
    FUN = mean)
  colnames(scores_rt) = c("sessionToken", "shiftingType", "rt")

  # reformat rt into wide format
  scores_rt = scores_rt %>%
    tidyr::pivot_wider(
      id_cols = sessionToken,
      names_prefix = "session_rt_",
      names_from = shiftingType,
      values_from = rt
    ) %>%
    # calc switch cost and mix cost
    dplyr::mutate(
      # switch cost = switch - repetition
      session_rt_switchcost = session_rt_switch - session_rt_repetition,
      # mix cost = repetition - single
      session_rt_mixcost = session_rt_repetition - session_rt_single
    )

  # merge back into master
  tatool = merge(tatool, tatool.rt, all = T) %>% merge(scores_rt, all = T); rm(tatool.rt); rm(scores_rt)





  ##### bin for switch -----

  # trim <200ms (all trials) or +-sdcutoff (throw accurate trials only, keep inaccurate +-sdcutoff as long as >=200ms)

  tatool.trimmed = tatool %>%
    dplyr::filter(
      reactionTime >= 200 &
        ((trial_rt_z > -sdcutoff & trial_rt_z < sdcutoff) | is.na(trial_rt_z))
    )

  # binning is only for switch trials so only keep switch trials

  tatool.trimmed.switch = tatool.trimmed %>% dplyr::filter(shiftingType == "switch")
  rm(tatool.trimmed)

  # each participant’s “baseline” reaction time was first determined by taking the mean of their reaction times on accurate repeat trials

  # has alr been calculated earlier: tatool.trimmed.switch$session_rt_repetition

  # Then, for each participant, this mean was subtracted from their accurate switch trials, such that the new score for each accurate switch trial, termed the switch cost, reflected how fast or slow the participant reacted compared to their own baseline.

  tatool.trimmed.switch.accurate = tatool.trimmed.switch %>%
    dplyr::filter(score == 1) %>%
    dplyr::mutate(trial_rt_switchcost = reactionTime - session_rt_repetition)

  # Afterwards, all switch costs across all participants were rank ordered, and based on deciles, were assigned a bin value from 1 (fastest 10%) to 10 (slowest 10%)

  tatool.trimmed.switch.accurate = tatool.trimmed.switch.accurate %>%
    dplyr::mutate(trial_bin_switchcost = ntile(trial_rt_switchcost, 10))

  tatool.trimmed.switch = merge(tatool.trimmed.switch, tatool.trimmed.switch.accurate, all = T)
  rm(tatool.trimmed.switch.accurate)

  # Next, as a penalty for inaccuracy, all inaccurate switch trials were assigned a bin value of 20 regardless of actual reaction time

  tatool.trimmed.switch[tatool.trimmed.switch$score == 0, "trial_bin_switchcost"] = 20

  # Lastly, a mean bin score was computed for each participant.

  tatool = merge(tatool, tatool.trimmed.switch, all = T) %>%
    group_by(sessionToken) %>%
    mutate(session_bin_switchcost = mean(trial_bin_switchcost, na.rm = T)) %>%
    ungroup()
  rm(tatool.trimmed.switch)





  ##### bin for mix -----

  # trim <200ms (all trials) or +-sdcutoff (throw accurate trials only, keep inaccurate +-sdcutoff as long as >=200ms)

  tatool.trimmed = tatool %>%
    dplyr::filter(
      reactionTime >= 200 &
        ((trial_rt_z > -sdcutoff & trial_rt_z < sdcutoff) | is.na(trial_rt_z))
    )

  # binning is only for repeat trials so only keep repeat trials

  tatool.trimmed.repeat = tatool.trimmed %>% dplyr::filter(shiftingType == "repetition")
  rm(tatool.trimmed)

  # each participant’s “baseline” reaction time was first determined by taking the mean of their reaction times on accurate single trials

  # has alr been calculated earlier: tatool.trimmed.switch$session_rt_single

  # Then, for each participant, this mean was subtracted from their accurate repeat trials, such that the new score for each accurate repeat trial, termed the mix cost, reflected how fast or slow the participant reacted compared to their own baseline.

  tatool.trimmed.repeat.accurate = tatool.trimmed.repeat %>%
    dplyr::filter(score == 1) %>%
    dplyr::mutate(trial_rt_mixcost = reactionTime - session_rt_single)

  # Afterwards, all switch costs across all participants were rank ordered, and based on deciles, were assigned a bin value from 1 (fastest 10%) to 10 (slowest 10%)

  tatool.trimmed.repeat.accurate = tatool.trimmed.repeat.accurate %>%
    dplyr::mutate(trial_bin_mixcost = ntile(trial_rt_mixcost, 10))

  tatool.trimmed.repeat = merge(tatool.trimmed.repeat, tatool.trimmed.repeat.accurate, all = T)
  rm(tatool.trimmed.repeat.accurate)

  # Next, as a penalty for inaccuracy, all inaccurate mix trials were assigned a bin value of 20 regardless of actual reaction time

  tatool.trimmed.repeat[tatool.trimmed.repeat$score == 0, "trial_bin_mixcost"] = 20

  # Lastly, a mean bin score was computed for each participant.

  tatool = merge(tatool, tatool.trimmed.repeat, all = T) %>%
    group_by(sessionToken) %>%
    mutate(session_bin_mixcost = mean(trial_bin_mixcost, na.rm = T)) %>%
    ungroup()
  rm(tatool.trimmed.repeat)





  ##### clean trial- and session-level dfs and return -----
  tatool = tatool[order(tatool$participantID, tatool$trialId), ]
  tatool = tatool %>% dplyr::select(participantID, sessionToken, session_format, tasknum, taskname, trialId, everything())
  tatool.session = tatool %>%
    distinct(sessionToken, .keep_all = T) %>%
    dplyr::select(
      participantID, session_format, tasknum, taskname,
      contains("session")
    ) %>% dplyr::select(-sessionToken)





  ##### write+return the two dfs -----
  if(write){
    write.csv(tatool, paste0("efScored_", task.exportlabel, "_triallevel.csv"))
    write.csv(tatool.session, paste0("efScored_", task.exportlabel, "_sessionlevel.csv"))
  }
  return(list(trial = tatool, session = tatool.session))

}



############################## PREPARE SCORED DATA FOR COMBINATION ##############################

prepForCombGeneral = function(scoredList, ef, number) {
  session.df = scoredList$session
  if(ef == "u") {
    if(number == 1) {
      out.df = session.df %>%
        dplyr::mutate(
          participantID = participantID,
          efU1colourACC = session_score,
          .keep = "none"
        )
    }
    if(number == 2) {
      out.df = session.df %>%
        dplyr::mutate(
          participantID = participantID,
          efU2letterACC = session_score,
          .keep = "none"
        )
    }
    if(number == 3) {
      out.df = session.df %>%
        dplyr::mutate(
          participantID = participantID,
          efU3numberACC = session_score,
          .keep = "none"
        )
    }
  }
  if(ef == "i" | ef == "ic") {
    if(number == 1) {
      out.df = session.df %>%
        dplyr::mutate(
          participantID = participantID,
          efI1simonACC = session_acc_interference,
          efI1simonRT = session_rt_interference,
          efI1simonBIN = session_bin_interference,
          .keep = "none"
        )
    }
    if(number == 2) {
      out.df = session.df %>%
        dplyr::mutate(
          participantID = participantID,
          efI2flankerACC = session_acc_interference,
          efI2flankerRT = session_rt_interference,
          efI2flankerBIN = session_bin_interference,
          efI2betterflankerACC = session_acc_betterinterference,
          efI2betterflankerRT = session_rt_betterinterference,
          efI2betterflankerBIN = session_bin_betterinterference,
          .keep = "none"
        )
    }
    if(number == 3) {
      out.df = session.df %>%
        dplyr::mutate(
          participantID = participantID,
          efI3stroopACC = session_acc_interference,
          efI3stroopRT = session_rt_interference,
          efI3stroopBIN = session_bin_interference,
          efI3betterstroopACC = session_acc_betterinterference,
          efI3betterstroopRT = session_rt_betterinterference,
          efI3betterstroopBIN = session_bin_betterinterference,
          .keep = "none"
        )
    }
  }
  if(ef == "ts" | ef == "s") {
    if(number == 1) {
      out.df = session.df %>%
        dplyr::mutate(
          participantID = participantID,
          efS1animacylocomotionACC = session_acc_switchcost,
          efS1animacylocomotionRT = session_rt_switchcost,
          efS1animacylocomotionBIN = session_bin_switchcost,
          efM1animacylocomotionACC = session_acc_mixcost,
          efM1animacylocomotionRT = session_rt_mixcost,
          efM1animacylocomotionBIN = session_bin_mixcost,
          .keep = "none"
        )
    }
    if(number == 2) {
      out.df = session.df %>%
        dplyr::mutate(
          participantID = participantID,
          efS2colourshapeACC = session_acc_switchcost,
          efS2colourshapeRT = session_rt_switchcost,
          efS2colourshapeBIN = session_bin_switchcost,
          efM2colourshapeACC = session_acc_mixcost,
          efM2colourshapeRT = session_rt_mixcost,
          efM2colourshapeBIN = session_bin_mixcost,
          .keep = "none"
        )
    }
    if(number == 3) {
      out.df = session.df %>%
        dplyr::mutate(
          participantID = participantID,
          efS3magnitudeparityACC = session_acc_switchcost,
          efS3magnitudeparityRT = session_rt_switchcost,
          efS3magnitudeparityBIN = session_bin_switchcost,
          efM3magnitudeparityACC = session_acc_mixcost,
          efM3magnitudeparityRT = session_rt_mixcost,
          efM3magnitudeparityBIN = session_bin_mixcost,
          .keep = "none"
        )
    }
  }
  return(out.df)
}



############################## CALCULATE SPLITHALF RELIABILITIES ##############################

getSplitHalf = function(cleaned_data, debug = FALSE) {

  # split data in half first (alternate rows)
  row_odd = seq_len(nrow(cleaned_data)) %% 2
  half.odd = cleaned_data[row_odd == 1, ]
  half.even = cleaned_data[row_odd == 0, ]

  # check what task it is so we know how to score
  tasknum = cleaned_data[1, "tasknum"]
  taskname = cleaned_data[1, "taskname"]

  # score data
  if(grepl("u", tasknum)) {
    getSplitHalf.helper = function(half_data) {
      half_data = scoreTatoolU(half_data)$trial %>%
        dplyr::select(participantID, trial_score) %>%
        dplyr::group_by(participantID) %>%
        dplyr::mutate(mean_score = mean(trial_score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(participantID, mean_score)
    }
  }
  if(grepl("ic", tasknum)) {
    getSplitHalf.helper = function(half_data) {
      half_data = scoreTatoolIC(half_data)$trial
      if(taskname == "simon") {half_data$trial_bin_betterinterference = NA_real_}
      half_data = half_data %>%
        dplyr::select(participantID, trial_bin_interference, trial_bin_betterinterference) %>%
        dplyr::group_by(participantID) %>%
        dplyr::mutate(
          mean_inter = mean(trial_bin_interference, na.rm = TRUE),
          mean_binter = mean(trial_bin_betterinterference, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(participantID, mean_inter, mean_binter)
    }
  }
  if(grepl("ts", tasknum)) {
    getSplitHalf.helper = function(half_data) {
      half_data = scoreTatoolTS(half_data)$trial %>%
        dplyr::select(participantID, trial_bin_switchcost, trial_bin_mixcost) %>%
        dplyr::group_by(participantID) %>%
        dplyr::mutate(
          mean_switch = mean(trial_bin_switchcost, na.rm = TRUE),
          mean_mix = mean(trial_bin_mixcost, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(participantID, mean_switch, mean_mix)
    }
  }

  half.odd = getSplitHalf.helper(half.odd)
  half.even = getSplitHalf.helper(half.even)
  scored = merge(half.odd, half.even, by = "participantID")
  is.nan.data.frame = function(x) do.call(cbind, lapply(x, is.nan))
  scored[is.nan(scored)] = NA
  if(debug) View(scored)

  # calc splithalf reliability with correction
  cat("\nSplit-half reliability (adjusted using the Spearman–Brown prophecy formula)\n")
  correction = function(r) {(2 * r) / (1 + r)}

  if(grepl("u", tasknum)) {
    r_adj = cor(scored[, 2], scored[, 3], use = "pairwise.complete.obs") %>% correction()
    cat("Updating Score:", r_adj, "\n")
  }
  if(grepl("ic", tasknum) | grepl("ts", tasknum)) {
    r_adj = cor(scored[, 2], scored[, 4], use = "pairwise.complete.obs") %>% correction()
    cat("    Interference bin (IC) or switch bin (TS):", r_adj, "\n")
    r_adj = cor(scored[, 3], scored[, 5], use = "pairwise.complete.obs") %>% correction()
    cat("Better interference bin (IC) or mix bin (TS):", r_adj, "\n")
  }

  return(invisible(NULL))

}