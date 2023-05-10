care_stage_module <- function(simObj) {

  #Remove dead people - these are the only quantity that are a bit odd in the
  #model, in that they exist the next month for just some of the modules - in
  #particular the transmission module (so that they can be left out)
  #Create stratified cd4 in order to merge costs that vary by cd4.
  popTrans <- simObj$popdf %>%
    mutate(floorage = floor(age),
           cd4strata = case_when(             cd4 < 50  ~ "<50",
                                 50  <= cd4 & cd4 < 100 ~ "50-99",
                                 100 <= cd4 & cd4 < 200 ~ "100-199",
                                 200 <= cd4 & cd4 < 350 ~ "200-349",
                                 350 <= cd4 & cd4 < 500 ~ "350-499",
                                 500 <= cd4             ~ "500+"))

  if (simObj$testflag) popTrans <- ensure(popTrans, all(.$stage != "dead"))

  #Merge in better and worse transition probabilities from the inputs
  transitionList <- simObj$stagetransprobs
  for (i in names(transitionList)) {

    #Perform gradual probability conversion to account for changes over time in
    #applicable care stage transition probabilities.
    if (i!="dead") transitionList[[i]] <-
        specific_month_prob(simObj$month, transitionList[[i]])

    #Generate merge groups based on the stage doing the merging
    if      (i == "hiv")      bygroup <- c("stage", "gender", "risk")
    else if (i == "diag")     bygroup <- c("stage", "oahsart")
    else if (i == "care")     bygroup <- c("stage")
    else if (i == "suppress") bygroup <- c("stage")
    else if (i == "left")     bygroup <- c("stage", "oahsart")
    else if (i == "dead")     bygroup <- c("cd4strata", "floorage")
    else stop(paste("Unexpected Stage:", i))

    #Merge transition probabilities for each stage to the population df.
    popTrans <- popTrans %>%
      left_join(transitionList[[i]], by = bygroup, suffix = c("",".y"))

    #Collapse probabilities into 4 columns after each merge.
    #This replaces the NA values.
    if (!(i %in% c(names(transitionList)[1], "dead"))) {
      popTrans <- popTrans %>%
        mutate(better = case_when(age >= 100    ~ 0,
                                  is.na(better) ~ better.y,
                                  TRUE          ~ better),
               worse  = case_when(age >= 100   ~ 0,
                                  is.na(worse) ~ worse.y,
                                  TRUE         ~ worse)) %>%
        select(-contains(".y"))
    }

  }

  #Merge in death from the inputs and manipulate it as needed for age and stage.
  #After that, update all values and calculate stay and then normalize to sum to 1.
  popTrans <- popTrans %>%
    mutate(death  = if_else(age >= 100, as.numeric(1), death)) %>%
    left_join(simObj$servtransmods,
              by = c("stage", "mcm", "mhsa", "support", "benemcm", "benemhsa",
                     "benesupport"))

  if (simObj$testflag) popTrans <-
    ensure(popTrans,
           all(!is.na(select(., better, worse, death))),
           all(!is.na(select(., bettermult, worsemult))))

  #Modify probabilities based on the transition modifiers - then scale them as
  #needed so that they sum to 1.
  popTrans <- popTrans %>%
    mutate(better  = better * bettermult,
           worse   = worse  * worsemult,
           stay    = pmax(0, 1 - better - worse - death, na.rm = TRUE),
           probsum = better + worse + stay,
           better  = if_else(probsum != 0, (better / probsum)*(1-death), 0),
           worse   = if_else(probsum != 0, (worse  / probsum)*(1-death), 0),
           stay    = if_else(probsum != 0, (stay   / probsum)*(1-death), 0))

  if (simObj$testflag) popTrans <-
    ensure(popTrans, all.equal(apply(select(., better, stay, worse, death),
                                     1, sum),
                               rep(1, nrow(.)),
                               tolerance = .Machine$double.eps))

  #Use probabilities to determine stage transitions, then actually perform the
  #transition.
  newPop <- popTrans %>%
    mutate(nextStage = MPRrcat(nrow(.),
                               as.matrix(select(., better, stay, worse, death))),
           nextStage = case_when(stage == "hiv"      & nextStage == "better" ~ "diag",
                                 stage == "diag"     & nextStage == "better" ~ "care",
                                 stage == "care"     & nextStage == "better" ~ "suppress",
                                 stage == "care"     & nextStage == "worse"  ~ "left",
                                 stage == "suppress" & nextStage == "better" ~ "care",
                                 stage == "suppress" & nextStage == "worse"  ~ "left",
                                 stage == "left"     & nextStage == "better" ~ "care",
                                 nextStage == "stay"                         ~ stage,
                                 nextStage == "death"                        ~ "dead",
                                 TRUE                                        ~ "ERROR"))

  if (simObj$testflag) newPop <- ensure(newPop, all(.$nextStage != "ERROR"))

  TEMP = filter(newPop, stage == "hiv", nextStage == "diag")
  diag_time_TEMP <- tibble(ID = pull(TEMP, id),
                           month = simObj$month,
                           event = "diagnosis",
                           cd4 = pull(TEMP, cd4))
  simObj$diag_time = bind_rows(simObj$diag_time,
                               diag_time_TEMP)

  TEMP = filter(newPop, stage == "suppress", nextStage == "care")
  diag_time_TEMP <- tibble(ID = pull(TEMP, id),
                           month = simObj$month,
                           event = "rebound",
                           cd4 = pull(TEMP, cd4))
  simObj$diag_time = bind_rows(simObj$diag_time,
                               diag_time_TEMP)

  TEMP = filter(newPop, stage == "left", nextStage == "care")
  diag_time_TEMP <- tibble(ID = pull(TEMP, id),
                           month = simObj$month,
                           event = "reengage",
                           cd4 = pull(TEMP, cd4))
  simObj$diag_time = bind_rows(simObj$diag_time,
                               diag_time_TEMP)

  TEMP = filter(newPop, stage == "diag", nextStage == "care")
  diag_time_TEMP <- tibble(ID = pull(TEMP, id),
                           month = simObj$month,
                           event = "linkage",
                           cd4 = pull(TEMP, cd4))
  simObj$diag_time = bind_rows(simObj$diag_time,
                               diag_time_TEMP)

  TEMP = filter(newPop, stage != "dead", nextStage == "dead")
  diag_time_TEMP <- tibble(ID = pull(TEMP, id),
                           month = simObj$month,
                           event = "death",
                           cd4 = pull(TEMP, cd4))
  simObj$diag_time = bind_rows(simObj$diag_time,
                               diag_time_TEMP)

  if (simObj$valflag) {
	  newPop <- newPop %>%
		  mutate(laststage = stage,
             stage     = nextStage,
             lastsupp  = if_else(stage == "suppress",
                                 as.integer(simObj$month), lastsupp))
  } else {
    newPop <- newPop %>%
      mutate(stage = nextStage)
  }

  #Limit dataset just to the columns that we carry through the entire simulation
  #(i.e. remove columns that were created just for this module).
  newPop <- select_at(newPop, colnames(simObj$popdf))

  #Update and validate the simulation object
  simObj$module <- "care_stage"

  if (simObj$testflag) {
    stopifnot(nrow(simObj$popdf) == nrow(newPop))
    if (simObj$valflag) {
      stopifnot(all(select(simObj$popdf, -stage, -age, -agegroup,
                         -lastsupp, -laststage) ==
                      select(newPop, -stage, -age, -agegroup,
                             -lastsupp, -laststage),
                    na.rm = TRUE))
    } else {
      stopifnot(all(select(simObj$popdf, -stage, -age, -agegroup) ==
                      select(newPop, -stage, -age, -agegroup),
                    na.rm = TRUE))
    }
  }

  #Assign the new population dataset to the simobj so that it is carried forward
  #in the simulation.
  simObj$popdf <- newPop

  #Return simulation object
  return(simObj)

}

