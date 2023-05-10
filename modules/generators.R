
#------------------------------------------------------------------------------#
#--- Called directly by input module to generate simulation pieces dependent
#--- upon user input.
#------------------------------------------------------------------------------#

gen_hiv_demo_dist <- function(origin) {
  #Provide a dataframe object that contains the default proportions of the
  #population in each care stage by age, gender, race, risk, and RWHAP
  #eligibility.
  read_demo_breakdown(origin, "stage")
}

gen_service_demo_dist <- function(origin, sens_filepath = NULL, sens_tabName = NULL) {
  #Provide a dataframe object that contains the default proportions of the
  #population in each care stage by age, gender, race, risk, and RWHAP
  #eligibility.
  read_demo_breakdown(origin,"service", sens_filepath, sens_tabName)
}

gen_at_risk_demo_dist <- function(origin) {
  #Provide a dataframe object that contains the default proportions of the
  #population in each care stage by age, gender, race, risk, and RWHAP
  #eligibility.
  read_demo_breakdown(origin, "risk")
}

gen_rwhap_by_stage_dist <- function(origin) {
  #Provide a dataframe object that contains the default proportions of the
  #population in each care stage overall
  defMults <- read_excel(origin,
                         sheet = "High Level Pop + Sim Features",
                         range = "B13:E22",
                         col_names = FALSE) %>%
    convert_unnamed() %>%
    fill(temp1) %>%
    mutate(rwhap = if_else(temp1 == "RWHAP", TRUE, FALSE)) %>%
    convert_stage_input("temp2") %>%
    rename(rwhapstagemult = temp4) %>%
    select(rwhap, stage, rwhapstagemult) %>%
    mutate(rwhapstagemult = rwhapstagemult/sum(rwhapstagemult))

  #Verify that percentages sum to 1
  stopifnot(all.equal(sum(defMults$rwhapstagemult), 1,
                      tolerance = .Machine$double.eps))

  return(defMults)
}

gen_rna_dist <- function(origin) {
  #Provide a dataframe object that contains the default proportions of the
  #population in each rnaset within stage

  .stages <- c("hiv", "diag", "care", "suppress", "left")
  .rnas <- c("lt200", "lt400", "lt3.5k", "lt10k", "lt50k", "gt50k")

  #Create dataframe of all possible combinations of merge variables and then use
  #as the basis for adding actual data in subsequent merges.
  defMults <- expand.grid(.stages,
                          .rnas,
                          KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE
    ) %>%
    `colnames<-`(c("stage", "rnaset"))

  #Read raw input from the inputs required for this function.
  rawInput <- read_excel(origin,
                         sheet     = "CD4 + Viral Load Features",
                         range     = "F6:H11",
                         col_names = FALSE) %>%
    convert_unnamed() %>%
    convert_rna_input("temp1") %>%
    rename(rnaset  = rna,
           rnamult = temp3) %>%
    select(rnaset, rnamult)

  defMults <- left_join(defMults, rawInput, by = "rnaset")

  #Scale rna probabilities to sum to 1 if they don't already.
  sumMult  <- defMults %>%
    distinct(rnaset, .keep_all = TRUE) %>%
    pull(rnamult) %>%
    sum()

  defMults <- mutate(defMults, rnamult = rnamult/!!sumMult)

  #Verify that percentages sum to 1
  stopifnot(all.equal(defMults %>%
                        distinct(rnaset, .keep_all = TRUE) %>%
                        pull(rnamult) %>%
                        sum(),
                      1,
                      tolerance = .Machine$double.eps))

  return(defMults)
}

gen_cd4_defs <- function(origin) {
  #Provide a dataframe object that contains the default cd4 mean and sd by
  #care stage
  rawInput <- read_excel(origin,
                         sheet     = "CD4 + Viral Load Features",
                         range     = "A6:D11",
                         na        = c("", NA),
                         col_names = FALSE) %>%
    convert_unnamed() %>%
    convert_stage_input("temp1") %>%
    rename(dist = temp2)

  #Bring precd4 wide since when it is used in the simulation everyone will need
  #a precd4 calculation, whereas current cd4 will be stage-specific.
  preCd4Info <- filter(rawInput, stage=="neg")[rep(1,nrow(rawInput)-1),] %>%
    rename(precd4param1 = temp3, precd4param2 = temp4, predist = dist) %>%
    select(predist, precd4param1, precd4param2)

  bind_cols(preCd4Info, filter(rawInput, stage!="neg")) %>%
    rename(cd4param1 = temp3, cd4param2 = temp4) %>%
    select(stage, predist, precd4param1, precd4param2,
           dist, cd4param1, cd4param2)
}

gen_trans_params <- function(origin) {

  #Load data required tables from input parameter workbook
  iduNetRaw <- read_excel(origin, "Transmission + At-Risk Features", "A3:B8")
  heteroNetRaw <- read_excel(origin, "Transmission + At-Risk Features", "A11:D25") %>%
    fill(Parameter, `Age Category`)
  msmNetRaw <- read_excel(origin, "Transmission + At-Risk Features", "A28:D42") %>%
    fill(Parameter, `Age Category`)

  # Transmission Parameters: IDU Probability for Contaminated Syringe
  trans_prob_IDU_contaminated <-
    iduNetRaw$Value[iduNetRaw$Parameter==
                      "Probability of Transmission Through Contaminated Syringe"]
  # Transmission Parameters: IDU Injection per Month
  trans_prob_IDU_per_month <-
    iduNetRaw$Value[iduNetRaw$Parameter=="Injections Per Month"]
  # Transmission Parameters: Percentage of contaminated syringes
  trans_prob_IDU_contaminated_per <-
    iduNetRaw$Value[iduNetRaw$Parameter==
                      "Percentage of Syringes With Contamination"]
  # Transmission Parameters: Partners Per Person Per Month
  trans_prob_IDU_num_partners <-
    iduNetRaw$Value[iduNetRaw$Parameter==
                      "Partners Per Person Per Month"]

  modify_sexual_partners <- function(data, construct) {
    if (construct=="formation") {
      filter(data, Parameter == "Sexual Partner Formation Rate") %>%
        convert_agegroup_input("Age Category") %>%
        rename(d_prob = Value) %>%
        select(agegroup, d_prob)
    } else if (construct=="dissolution") {
      filter(data, Parameter == "Sexual Partner Dissolution Rate") %>%
        convert_agegroup_input("Age Category") %>%
        rename(Poisson_mean = Value) %>%
        select(agegroup, Poisson_mean)
    }
  }

  list("discordant_IDU"    = iduNetRaw$Value[iduNetRaw$Parameter==
                                               "Discordant Multiplier"],
       "discordant_MSM"    = msmNetRaw$Value[msmNetRaw$Parameter==
                                               "Discordant Multiplier"],
       "discordant_nonMSM" = heteroNetRaw$Value[heteroNetRaw$Parameter==
                                                  "Discordant Multiplier"],
       # Transmission Parameters: Partners per Person via IDU
       "Poisson_mean_IDU"    = iduNetRaw$Value[iduNetRaw$Parameter==
                                                 "Partners Per Person Per Month"],
       # Transmission Parameters: Partners per Person via MSM Sex
       "Poisson_mean_MSM"    = msmNetRaw$Value[msmNetRaw$Parameter==
                                                 "Partners Per Person Per Month"],
       # Transmission Parameters: Partners per Person via nonMSM Sex
       "Poisson_mean_nonMSM" = heteroNetRaw$Value[heteroNetRaw$Parameter==
                                                 "Partners Per Person Per Month"],
       "trans_prob_IDU"      = (1-(1-(trans_prob_IDU_contaminated))^(
         trans_prob_IDU_per_month * trans_prob_IDU_contaminated_per *
           (1/trans_prob_IDU_num_partners))),
       # Transmission Parameters: MSM Sexual Partner formation by age category
       "d_param_MSM"    = modify_sexual_partners(msmNetRaw,    "formation"),
       # Transmission Parameters: nonMSM Sexual Partner formation by age category
       "d_param_nonMSM" = modify_sexual_partners(heteroNetRaw, "formation"),
       # Transmission Parameters: MSM Sexual Partner dissolution by age category
       "f_param_MSM"    = modify_sexual_partners(msmNetRaw,    "dissolution"),
       # Transmission Parameters: nonMSM Sexual Partner dissolution by age category
       "f_param_nonMSM" = modify_sexual_partners(heteroNetRaw, "dissolution"),
       # Transmission Probabilities by HIV-RNA
       "trans_prob_RNA_MSM" =
         filter(msmNetRaw,
                Parameter ==
                  "HIV Monthly Transmission Probability Between Partners") %>%
         convert_rna_input("Viral Load") %>%
         mutate(trans_prob = Value) %>%
         select(rna, trans_prob),
       "trans_prob_RNA_nonMSM" =
         filter(heteroNetRaw,
                Parameter ==
                  "HIV Monthly Transmission Probability Between Partners") %>%
         convert_rna_input("Viral Load") %>%
         mutate(trans_prob = Value) %>%
         select(rna, trans_prob)
  )
}

gen_costs_qaly <- function(origin, sens_costVal = NULL, sens_costValName = NULL, sens_qalyVals = NULL) {

  #Get raw values from input spreadsheet
  rawCosts <- read_costs(origin, sens_costVal, sens_costValName)

  #Expand raw values as needed so that they can be merged to the population
  #dataframe
  cd4Costs <- bind_rows(mutate(rawCosts$rawcd4costs, stage = "hiv"),
                        mutate(rawCosts$rawcd4costs, stage = "diag"),
                        mutate(rawCosts$rawcd4costs, stage = "care"),
                        mutate(rawCosts$rawcd4costs, stage = "suppress"),
                        mutate(rawCosts$rawcd4costs, stage = "left"),
                        mutate_at(rawCosts$rawcd4costs, vars(contains("cost")),
                                  list(~as.numeric(0))) %>%
                          mutate(stage = "dead")) %>%
    mutate_at(vars(contains("cost")), list(~./12))

  #Create mergable costs table that can be merged onto every record based on
  #stage.  In other words, since most variables are created a priori, they
  #already exist on the record even if the record isn't 'eligible' for them yet.
  #As such, make their 'cost' 0 for 'ineligible' records.  Also change costs
  #from annual to monthly.
  servCosts <- gen_base(.stages    = c("hiv", "diag", "care", "suppress", "left", "dead"),
                        .rservices = c("mcm", "mhsa", "support")) %>%
    left_join(rawCosts$rawservcosts %>%
                .[rep(1, 6),] %>%
                mutate(stage = c("hiv", "diag", "care",
                                 "suppress", "left", "dead")),
              by = "stage") %>%
    mutate_at(vars(contains("cost")),
              list(~if_else(stage %in% c("care", "suppress"), ., 0))) %>%
    mutate(mcmcost     = mcm*mcmcost,
           mhsacost    = mhsa*mhsacost,
           supportcost = support*supportcost) %>%
    mutate_at(vars(contains("cost")), list(~./12))

  #Change qaly from a yearly measure to a monthly one.
  qalyRaw <- read_excel(origin, "Costs + Quality-of-Life Weights", "E4:F8",
                     col_names = FALSE) %>%
    convert_unnamed() %>%
    rename("cd4strata" = "temp1") %>%
    mutate("qaly" = temp2/12) %>%
    select(-temp2)

  if (!is.null(sens_qalyVals)) qalyRaw$qaly <- sens_qalyVals/12

  #Create a mergable version of the qaly information by stage - which is simply
  #repeating the same values for every stage-cd4 combination.
  qaly <- bind_rows(mutate(qalyRaw, stage = "hiv"),
                    mutate(qalyRaw, stage = "diag"),
                    mutate(qalyRaw, stage = "care"),
                    mutate(qalyRaw, stage = "suppress"),
                    mutate(qalyRaw, stage = "left"),
                    mutate(qalyRaw, stage = "dead", qaly = 0))

  return(list(cd4costs  = cd4Costs,
              servcosts = servCosts,
              qaly      = qaly))
}

gen_stage_transition_probs <- function(origin,
                                       sens_name = NULL,
                                       sens_transVal = NULL,
                                       sens_transProbsRaw_rownum = NULL,
                                       sens_transDeath = NULL) {
  #Load inputs corresponding to transition probabilities and then organize them
  #into the same format for stacking to create one mergable data table.

  #Supporting functions
  reshape_oahsart_only_better <- function(data, from) {
    #Reshape inputs that are conditional on oahsart.
    #Fill oahsart==0 with 0s.
    filter(data, stage==!!from) %>%
      mutate(oahsart = 0,
             betters = 0,
             bettere = 0,
             btime   = 0) %>%
      bind_rows(filter(data, stage==!!from) %>%
                  rename(betters = temp7,
                         bettere = temp8,
                         btime   = temp9) %>%
                  mutate(oahsart = 1)) %>%
      select(stage, oahsart, betters, bettere, btime) %>%
      mutate(worses = 0,
             worsee = 0,
             wtime  = 0)
  }

  reshape_better_worse <- function(data, from, better, worse) {
    #Reshape for transition probabilities that have a better and worse that is
    #not conditional on oahsart.
    left_join(filter(data, stage==!!from, to == !!better) %>%
                rename(betters = temp7,
                       bettere = temp8,
                       btime   = temp9) %>%
                select(stage, betters, bettere, btime),
              filter(data, stage==!!from, to == !!worse) %>%
                rename(worses = temp7,
                       worsee = temp8,
                       wtime  = temp9) %>%
                select(stage, worses, worsee, wtime), by = "stage")
  }

  #raw
  transProbsRaw <- read_excel(origin, "Transition Probabilities", "A4:I15",
                              col_names = FALSE) %>%
    convert_unnamed() %>%
    fill(temp1, temp3, temp5) %>%
    convert_stage_input("temp3") %>%
    rename(to = stage) %>%
    convert_stage_input("temp1") %>%
    mutate(temp9 = 12*temp9) %>%
    select(-temp2, -temp4)

  if ((!is.null(sens_transVal)) && (!is.null(sens_name))){
    if (sens_name == "Stage 2 to 3") transProbsRaw[1:6, "temp8"] <- sens_transVal
  }
  else if (!is.null(sens_transVal)){
    transProbsRaw[sens_transProbsRaw_rownum, 'temp8'] <- sens_transVal %>% as.numeric()
  }

  #hiv
  hiv <- filter(transProbsRaw, stage=="hiv") %>%
    convert_risk_input("temp6") %>%
    convert_gender_input("temp5") %>%
    rename(betters = temp7,
           bettere = temp8,
           btime   = temp9) %>%
    select(gender, risk, stage, betters, bettere, btime) %>%
    mutate(worses = 0,
           worsee = 0,
           wtime  = 0)

  #diag
  diag <- reshape_oahsart_only_better(transProbsRaw, "diag")

  #care
  care <- reshape_better_worse(transProbsRaw, "care", "suppress", "left")

  #suppress
  suppress <- reshape_better_worse(transProbsRaw, "suppress", "care", "left")

  #left
  left <- reshape_oahsart_only_better(transProbsRaw, "left")

  #dead
  deadLabels <- c("cd4strata",
                  read_excel(origin, "Transition Probabilities", "K4:P4",
                             col_names = FALSE) %>% unlist() %>% .[2:6])

  if (is.null(sens_transDeath)) {
    dead <- read_excel(origin, "Transition Probabilities", "K6:P11",
                       col_names = FALSE)
  }else{
    dead <- sens_transDeath
  }

  #Reshape dead and create a table for every single age-strata combination.
  #This facilitates merging in the actual simulation.
  dead <- dead%>%
    `colnames<-`(deadLabels) %>%
    gather("key", "death", -cd4strata) %>%
    separate(key, c("minage", "maxage"), convert = TRUE) %>%
    mutate(maxage = if_else((minage == 55) & (is.na(maxage)),
                            as.integer(100), maxage)) %>%
    .[rep(seq_len(nrow(.)), .$maxage-.$minage+1),] %>%
    group_by(cd4strata, minage, maxage) %>%
    mutate(floorage = minage + row_number() - 1) %>%
    ungroup() %>%
    select(cd4strata, floorage, death)

  return(list("hiv"      = hiv,
              "diag"     = diag,
              "care"     = care,
              "suppress" = suppress,
              "left"     = left,
              "dead"     = dead))

}

gen_serv_transition_mods <- function(origin,
                                     sens_unmetName = NULL,
                                     sens_unmetTransVal = NULL) {
  #Process the transition probability modifiers as a result of service receipt
  #(or not).

  #Supporting function
  extract_modified_stages <- function(origin, cell) {
    #Fill in data as needed to account for the merged cells and general
    #conversion from human-readable to machine-readable format.
    stuff <- lapply(unlist(str_split(
      read_cell(origin, "Transition Probabilities", cell), "\r\n")),
      function(x) str_split(x, " to "))
    from <- lapply(stuff, function(x) trimws(x[[1]][1])) %>% unlist()
    to   <- lapply(stuff, function(x) trimws(x[[1]][2])) %>% unlist()
    as.data.frame(list("from" = from,
                       "to"   = to)) %>%
      convert_stage_input("to") %>%
      rename(change = stage) %>%
      convert_stage_input("from") %>%
      convert_unnamed()
  }

  #Use the modifiers to determine if something should be a modifier of the
  #'better' or 'worse' categories.
  modifiedStages <- bind_rows(extract_modified_stages(origin, "A25") %>%
                                mutate(temp1 = "Increase"),
                              extract_modified_stages(origin, "A18") %>%
                                mutate(temp1 = "Decrease"),
                              extract_modified_stages(origin, "A32") %>%
                                mutate(temp1 = "Increase_")) %>% #Needs to be different for the gather/spread
    mutate(change =
             case_when(stage == "care"     & change == "left"     ~ "worsemult",
                       stage == "care"     & change == "suppress" ~ "bettermult",
                       stage == "suppress" & change == "care"     ~ "bettermult",
                       stage == "suppress" & change == "left"     ~ "worsemult"))

  #Start with a frame to merge everything to and then apply logic to create the
  #merge variable
  serviceBase <- gen_base(.stages   = c("hiv", "diag", "care", "suppress",
                                        "left", "dead"),
                          .services = c("mcm", "mhsa", "support")) %>%
    filter(!((mcm & !benemcm) |
               (mhsa & !benemhsa) |
               (support & !benesupport))) %>%
    mutate(temp2 = case_when(benemcm     & !mcm     &
                            benemhsa    & !mhsa    &
                            benesupport & !support ~
                              "MCM + MH/SA + Support and Other Core Services",
                            benemcm     & !mcm     &
                            benemhsa    & !mhsa    ~ "MCM + MH/SA",
                            benemcm     & !mcm     &
                            benesupport & !support ~
                              "MCM + Support and Other Core Services",
                            benemhsa    & !mhsa    &
                            benesupport & !support ~
                              "MH/SA + Support and Other Core Services",
                            benemcm     & !mcm     ~ "MCM",
                            benemhsa    & !mhsa    ~ "MH/SA",
                            benesupport & !support ~
                              "Support and Other Core Services"))

  #Pull in the actual values and replace the programatic Increase vs Increase_
  #distinction.
  rawMods <- read_excel(origin, "Transition Probabilities", "E18:I38",
             col_names = FALSE) %>%
    convert_unnamed() %>%
    mutate(temp1 = if_else(row_number()==15 & temp1=="Increase",
                           "Increase_", temp1)) %>%
    fill(temp1)

  if ((!is.null(sens_unmetTransVal)) && (!is.null(sens_unmetName))){
    if (sens_unmetName == "Stage 4 5 to 6"){
      rawMods[8:14, "temp5"] <- sens_unmetTransVal
      } else if (sens_unmetName == "Stage 4 to 5") {
        rawMods[1:7, "temp5"] <- sens_unmetTransVal
      }
  }

  #Create multiplier that will modify whatever transition probability the
  #individual has in the correct direction for increase/decrease.  Since this
  #will merge to all individuals, the multiplier is simply 1 for inapplicable
  #combinations of stage-services.
  rawMods <- rawMods %>%
    left_join(modifiedStages, by = "temp1") %>%
    mutate(mult = if_else(temp1 == "Decrease", 1-temp5, 1+temp5)) %>%
    select(temp2, stage, change, mult) %>%
    spread(change, mult)

  serviceBase %>%
    left_join(rawMods, by = c("stage", "temp2")) %>%
    mutate(bettermult = if_else(is.na(bettermult), as.numeric(1), bettermult),
           worsemult  = if_else(is.na(worsemult),  as.numeric(1), worsemult)) %>%
    select(stage, mcm, mhsa, support, benemcm, benemhsa, benesupport,
           bettermult, worsemult) %>%
    arrange(stage, mcm, mhsa, support, benemcm, benemhsa, benesupport)

}

#------------------------------------------------------------------------------#
#--- Supporting functions for the functions above
#------------------------------------------------------------------------------#

gen_demo_dist_base <- function() {
  .agegroups <- c("youth", "adult", "olderadult")
  .genders   <- c("male", "female")
  .races     <- c("black", "hispanic", "other")
  .risks     <- c("MSM","IDU", "MSMandIDU", "other")

  #Build dataframe with the correct shape to receive hard-coded proportions.
  gen_base(.agegroups, .genders, .races, .risks)
}

gen_base <- function(.agegroups = NA,
                     .genders   = NA,
                     .races     = NA,
                     .risks     = NA,
                     .stages    = NA,
                     .rnas      = NA,
                     .services  = NA,
                     .nservices = .services,
                     .rservices = .services) {
  #Outputs a dataframe that is ordered column-wise to
  #the tables in the inputs memo.

  #Breakout services as needed
  serviceList <- list()
  for (i in c(.rservices, paste0("bene", .nservices))) {
    if (!(i %in% c("beneoahsart", "beneNA")) & !is.na(i)) {
      serviceList[[i]] <- c(0,1)
    }
  }

  #Create dataset
  outdf <- expand.grid(c(list("agegroup" = .agegroups,
                              "gender"   = .genders,
                              "race"     = .races,
                              "risk"     = .risks,
                              "stage"    = .stages,
                              "rna"      = .rnas),
                         serviceList
                       ),
                       KEEP.OUT.ATTRS = FALSE
    ) %>%
    arrange(agegroup, gender, race, risk, stage, rna, !!!syms(names(serviceList))) %>%
    select_if(function(x) all(!is.na(x))) %>%
    mutate_if(is.factor, as.character)

  if (ncol(outdf)==0) {
    stop("Called gen_base but did not select any attributes.")
  }

  return(outdf)
}

#------------------------------------------------------------------------------#
#--- Supporting functions converting from human-readable to machine-readable
#------------------------------------------------------------------------------#

convert_stage_input <- function(data, XVar) {
  #Convert human-readable stage label to programmatic version
  mutate(data,
         stage = case_when((!!sym(XVar)) == "Stage 1: HIV Negative"       ~ "neg",
                           (!!sym(XVar)) == "Stage 2: Undiagnosed"        ~ "hiv",
                           (!!sym(XVar)) == "Stage 3: Diagnosed"          ~ "diag",
                           (!!sym(XVar)) == "Stage 4: Care and Treatment" ~ "care",
                           (!!sym(XVar)) == "Stage 5: Viral Suppression"  ~ "suppress",
                           (!!sym(XVar)) == "Stage 6: Left Care"          ~ "left")) %>%
    select(-!!sym(XVar))
}

convert_agegroup_input <- function(data, XVar) {
  #Convert human-readable agegroup label to programmatic version
  mutate(data,
         agegroup = case_when((!!sym(XVar)) == "Youth"       ~ "youth",
                              (!!sym(XVar)) == "Adult"       ~ "adult",
                              (!!sym(XVar)) == "Older Adult" ~ "olderadult")) %>%
    select(-!!sym(XVar))
}

convert_rna_input <- function(data, XVar) {
  #Convert human-readable rna label to programmatic version
  mutate(data,
         rna = case_when((!!sym(XVar)) == "<200"            ~ "lt200",
                         (!!sym(XVar)) == "200 - 400"       ~ "lt400",
                         (!!sym(XVar)) == "400 - 3,499"     ~ "lt3.5k",
                         (!!sym(XVar)) == "3,500 - 9,999"   ~ "lt10k",
                         (!!sym(XVar)) == "10,000 - 49,999" ~ "lt50k",
                         (!!sym(XVar)) == "50,000+"         ~ "gt50k")) %>%
    select(-!!sym(XVar))
}

convert_risk_input <- function(data, XVar) {
  #Convert human-readable risk label to programmatic version
  mutate(data,
         risk = case_when((!!sym(XVar)) == "MSM (No IDU)" ~ "MSM",
                          (!!sym(XVar)) == "IDU (No MSM)" ~ "IDU",
                          (!!sym(XVar)) == "IDU"          ~ "IDU",
                          (!!sym(XVar)) == "MSM and IDU"  ~ "MSMandIDU",
                          (!!sym(XVar)) == "Other"        ~ "other")) %>%
    select(-!!sym(XVar))
}

convert_gender_input <- function(data, XVar) {
  #Convert human-readable gender input to programmatic version
  mutate(data,
         gender = case_when((!!sym(XVar)) == "Male"   ~ "male",
                            (!!sym(XVar)) == "Female" ~ "female")) %>%
    select(-!!sym(XVar))
}
