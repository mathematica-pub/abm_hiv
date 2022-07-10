
initialization_module <- function(inputObj) {

  #---Declare Possible Variable Values---

  agegroups      <- c("youth", "adult", "olderadult")
  genders        <- c("male", "female")
  races          <- c("black", "hispanic", "other")
  risks          <- c("MSM","IDU", "MSMandIDU", "other")
  stages         <- c("hiv", "diag", "care", "suppress", "left")
  rnas           <- c("lt200", "lt400", "lt3.5k", "lt10k", "lt50k", "gt50k")
  services       <- c("mcm", "mhsa", "support")
  stats          <- c("precd4", "cd4")

  #---Create data shell of all possible individuals that the model can see---

  #Each service becomes two flags except oahsart:
  #1. receipt
  #2. benefit
  #oahsart would benefit everyone with hiv so there is only a receipt flag.
  serviceList <- list()
  for (i in c("oahsart", services, paste0("bene", services))) {
    serviceList[[i]] <- c(0,1)
  }

  #Create list of variables for which we will take all combinations to build
  #the dataframe with all possible individuals
  gridList <- c(list("agegroup" = agegroups,
                     "gender"   = genders,
                     "race"     = races,
                     "risk"     = risks,
                     "stage"    = stages,
                     "rnaset"   = rnas,
                     "rwhap"    = c(0,1)),
                serviceList)

  #Build data shell and remove inconsistencies within the demographic combinations
  dataShell <- expand.grid(gridList,
                           KEEP.OUT.ATTRS = FALSE,
                           stringsAsFactors = FALSE) %>%
    filter(!(gender == "female" & grepl("MSM", risk)))

  #---Begin calculating the number of individuals of each type by       ---
  #---subsetting the total population into smaller and smaller subgroups---

  #Combine all possible individual types with the input proportions
  combinedDemos <- left_join(inputObj$popprobs$hiv, inputObj$popprobs$service,
                             by = c("agegroup", "gender", "race", "risk", "rwhap")) %>%
    left_join(inputObj$popprobs$nodes, by = "rwhap") %>%
    left_join(inputObj$popprobs$rna, by = "stage")

  if (inputObj$testflag) combinedDemos <- ensure(combinedDemos, sum(is.na(.))==0)

  hivData <- dataShell %>%
    left_join(combinedDemos, by = c("agegroup", "gender", "race", "risk",
                                    "stage", "rnaset", "rwhap")) %>%
    mutate(totpop = inputObj$pospopulationsize,
           pop    = as.numeric(NA))

  if (inputObj$testflag) hivData <- ensure(hivData, nrow(.)==nrow(dataShell))

  #Subset population by eligibility*stage and the number of people in that
  #eligibility-stage combination by agegroup, gender, race, risk, rnaset
  #category
  for (i in stages) {
    hivData <- mutate(hivData,
                      pop = case_when(
                        stage == !!i ~ totpop*rwhapstagemult*rnamult*!!sym(i),
                        TRUE         ~ pop))
  }

  #Verify subsetting has not added to or subtracted from the population
  checkthis <- hivData %>%
    select(agegroup, gender, race, risk, stage, rwhap, rnaset, pop) %>%
    distinct(agegroup, gender, race, risk, stage, rwhap, rnaset,
             .keep_all = TRUE)

  if (inputObj$testflag) stopifnot(all.equal(sum(checkthis$pop),
                                             inputObj$pospopulationsize,
                                             tolerance = 0.5))

  #Remove multipliers that have been used from the dataset and create a new
  #population variable to store the results of the next subsetting procedure
  hivData <- hivData %>%
    select_at(vars(setdiff(colnames(.),
                           c("rwhapstagemult", "rnamult", stages)))) %>%
    mutate(newpop = pop)

  #Loop over services, generating and then applying subset multipliers.
  #These multipliers subset the eligibility-stage-age-gender-race-risk-rnaset
  #populations further by whether or not the population benefits from the
  #service and whether or they received that service.  Each iteration of a loop
  #subsets by a further service.  This loop takes the data to the most
  #granular level, which if using the defaults, is:
  #eligibility-stage-age-gender-race-risk-rnaset-benemcm-mcm-benemhsa-mhsa-benesupport-support
  for (i in services) {
    hivData <-
      mutate(hivData,
             !!sym(paste0(i, "needmult")) :=
               case_when(as.logical(!!sym(paste0("bene", i))) ~
                           !!sym(paste0(i, "need")),
                         !as.logical(!!sym(paste0("bene", i))) ~
                           1 - !!sym(paste0(i, "need"))),
             !!sym(paste0(i, "receiptmult")) :=
               case_when(as.logical(!!sym(paste0("bene", i))) &
                           as.logical(!!sym(i)) ~ !!sym(paste0(i, "receipt")),
                         as.logical(!!sym(paste0("bene", i))) &
                           !as.logical(!!sym(i)) ~ 1 - !!sym(paste0(i, "receipt")),
                         !as.logical(!!sym(paste0("bene", i))) &
                           as.logical(!!sym(i)) ~ 0,
                         !as.logical(!!sym(paste0("bene", i))) &
                           !as.logical(!!sym(i)) ~ 1),
             newpop = newpop*(!!sym(paste0(i, "needmult")))*
               (!!sym(paste0(i, "receiptmult"))))
  }

  hivData <- hivData %>%
    mutate("oahsartreceiptmult" = if_else(as.logical(oahsart),
                                          oahsartreceipt,
                                          1 - oahsartreceipt),
           newpop = newpop*oahsartreceiptmult)

  #Verify subsetting has not added or subtracted from the population
  if (inputObj$testflag) stopifnot(all.equal(sum(hivData$newpop),
                                             inputObj$pospopulationsize,
                                             tolerance = 0.5))

  #Remove variables that are no longer needed
  hivData <- filter(hivData, newpop != 0) %>%
    select(-contains("mult"), -contains("receipt"), -contains("need"))

  #Verify that none of the population has been assigned to individuals that
  #received a service that they could not benefit from.
  if (inputObj$testflag) {
    stopifnot(nrow(filter(hivData, mcm & !benemcm)) == 0)
    stopifnot(nrow(filter(hivData, mhsa & !benemhsa)) == 0)
    stopifnot(nrow(filter(hivData, support & !benesupport)) == 0)
  }


  #---Clear up service and oahsart-stage inconsistencies---

  #Make dataset make sense based on oahsart, then combine records that now
  #have identical characteristics.
  hivDataAgg <- hivData %>%
    mutate(mcm     = if_else(!oahsart, 0, mcm),
           mhsa    = if_else(!oahsart, 0, mhsa),
           support = if_else(!oahsart, 0, support),
           stage   = if_else(!oahsart & stage %in%
                               c("care", "suppress"), "left", stage)) %>%
    group_by_at(vars(setdiff(colnames(.), c("totpop", "pop", "newpop")))) %>%
    summarize(pop = sum(newpop), .groups = "drop")

  #Verify that correcting errors has not changed the population size.
  if (inputObj$testflag) stopifnot(all.equal(sum(hivDataAgg$pop),
                                             inputObj$pospopulationsize,
                                             tolerance = 0.5))

  #Dropping variables that are unneeded after initialization to combine as
  #many "types" of individual as possible before splitting out rows based on
  #counts.
  hivDataAgg <- hivDataAgg %>%
    group_by_at(vars(setdiff(colnames(.), "pop"))) %>%
    summarize(pop = sum(pop), .groups = "drop")

  if (inputObj$testflag) hivDataAgg <- ensure(hivDataAgg,
                                              all.equal(sum(.$pop),
                                                        inputObj$pospopulationsize,
                                                        tolerance = 0.5))

  #Blow-up the dataset to the population size
  #NOTE: In order for this code to work, the population associated with each
  #possible individual needs to be an integer.  After rounding the population
  #may not be exactly the same size as the originally selected population.
  #(we can't have partial people running around!)  To correct this, we manually
  #round up the largest "partial people" until we hit the user's desired number.
  conversionData <- hivDataAgg %>%
    mutate(roundpop = round(pop))

  chkSize <- conversionData %>%
    pull(roundpop) %>%
    sum()

  #Number of manual roundups needed
  missSize <- inputObj$pospopulationsize-chkSize

  #Pull out the biggest partial people
  conversionDataMan <- conversionData %>%
    arrange(roundpop, desc(pop)) %>%
    mutate(roundpop = if_else(row_number() <= missSize, 1, roundpop))

  #Now blow-up dataset
  fullPop <- conversionDataMan %>%
    .[rep(seq_len(nrow(.)), .$roundpop),] %>%
    select(-pop, -roundpop)

  if (inputObj$testflag) fullPop <-
    ensure(fullPop, nrow(fullPop)==inputObj$pospopulationsize)

  #---Generate cd4 counts and precounts for all individuals---

  #Join the cd4 information onto the individuals, then sample the counts from
  #the appropriate normal distributions.  Finally, check that any NA values
  #that are generated by the cd4 rnorm function only occur when the mean or
  #sd are missing, and that the mean and sd are only missing for the suppress
  #stage.
  if (inputObj$testflag) stopifnot(inputObj$popcd4dists$predist=="Normal")
  fullPop <- fullPop %>%
    ungroup() %>%
    left_join(inputObj$popcd4dists, by = c("stage")) %>%
    mutate(precd4 = rtruncnorm(nrow(.), a = 0,
                               mean = precd4param1, sd = precd4param2))

  suppressWarnings(fullPop <- fullPop %>%
    mutate(cd4rev = case_when(stage == "suppress"          ~ NA_real_,
                              dist == "Mirrored Lognormal" ~ rlnorm(nrow(.),
                                                                    cd4param1,
                                                                    cd4param2),
                              dist == "Mirrored Gamma"     ~ rgamma(nrow(.),
                                                                    cd4param1,
                                                                    cd4param2)),
           cd4rev = if_else(cd4rev > precd4,
                            case_when(stage == "suppress"          ~ NA_real_,
                                      dist == "Mirrored Lognormal" ~ rlnorm(nrow(.),
                                                                            cd4param1,
                                                                            cd4param2),
                                      dist == "Mirrored Gamma"     ~ rgamma(nrow(.),
                                                                            cd4param1,
                                                                            cd4param2)),
                            cd4rev)))

  fullPop <- fullPop %>%
    mutate(cd4 = case_when(stage == "suppress" ~ precd4,
                           TRUE                ~ pmin(pmax(0, (cd4rev-max(precd4))*(-1)),
                                                      precd4)))

  if (inputObj$testflag) fullPop <- ensure(fullPop,
                                           all(is.na(.$cd4param1[.$stage == "suppress"])),
                                           all(is.na(.$cd4param2[.$stage == "suppress"])),
                                           all(!is.na(.$cd4[.$stage != "suppress"])))

  #---Generate age for everyone in the model---

  fullPop <- fullPop %>%
    gen_age() %>%
    gen_agegroup()

  #---Generate infection duration, month, and appropriate rna values---

  #Duration is determined by the difference between cd4 and precd4 counts.
  #Infection month is determined by the duration.
  #rna level is determined by ac ombination of the rnaset level, infection
  #duration, and care stage
  #Finally, unneeded variables are dropped and the data set is rearranged for
  #human readability.
  fullPop <- fullPop %>%
    mutate(infectdur = if_else(stage == "suppress", NA_real_,
                               floor((precd4-cd4) / !!inputObj$cd4decrease)),
           infectmon = 0 - infectdur,
           rna       = case_when(infectdur == 0      ~ "gt50k",
                                 stage == "suppress" ~ "lt200",
                                 TRUE                ~ as.character(rnaset)),
           id        = row_number())  %>%
    select(-contains("param1"), -contains("param2"), -contains("dist"), -cd4rev)

  if (inputObj$valflag) {
    fullPop <- fullPop %>%
      mutate(lastsupp  = if_else(stage == "suppress", as.integer(0), NA_integer_),
             laststage = NA_character_)
  }

  preSimObj <- c(inputObj,
                 list("month"       = 0,
                      "module"      = "initialization",
                      "popdf"       = fullPop,
                      "popdenom"    = ((nrow(fullPop)/
                                         inputObj$actualpospopsize)*
                                         inputObj$uspopulation)))

  #---Generate Negative Population---

  preSimObj <- initialize_negative_pop(preSimObj)

  #---Generate Transmission Networks---

  simObj <- c(preSimObj,
              list("networks" =
                     list(IDU_Net      = gen_risk_net(preSimObj, "IDU"),
                          S_MSM_Net    = gen_risk_net(preSimObj, "MSM"),
                          S_nonMSM_Net = rbind(gen_gender_net(preSimObj, "female"),
                                               gen_gender_net(preSimObj, "male")))))

  if (simObj$testflag) check_assert_that(simObj,
                                         New_infection.df = NULL,
                                         checks =c(1:9))

  return(simObj)
}
