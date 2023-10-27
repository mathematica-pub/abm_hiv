
#------------------------------------------------------------------------------#
#--- Functions for supporting loading of inputs in the workbook config file
#------------------------------------------------------------------------------#

convert_unnamed <- function(data) {
  #When data is read in, if it doesn't come with column headers then the tibble
  #package will assign a temp name.  The temp name differs depending on the
  #version of tibble that the user is using.  As such, this helper function
  #accounts for whatever temp names are created and converts them to a
  #consistent format that can be expected.
  data %>%
    `colnames<-`(gsub("^(X__|(\\.){2,3})([0-9]?)$", "temp\\3", colnames(.))) %>%
    return()
}

assign_rwhap_sheet <- function(rwhap, suffix) {
  #Helper function to map the logical rwhap variable to the human-readable input
  #workbook equivalent.
  if      (rwhap == TRUE)  sheet <- paste("RWHAP",     suffix)
  else if (rwhap == FALSE) sheet <- paste("Non-RWHAP", suffix)
  else                     stop("'rwhap' must be of type logical.")
  return(sheet)
}

read_demo_matrix <- function(filepath, rwhap, sheet, range, charVar) {
  #Function for reading the prototypical demographic matrix format used
  #throughout the user input workbook and converting it to a more amenable
  #format for working with programmatically.
  suppressMessages(
    read_excel(filepath, sheet = sheet, range = range, col_names = FALSE,
             na = c("", "-")) %>%
    convert_unnamed() %>%
    rename("youth_male"      = "temp1", "youth_female"      = "temp2",
           "adult_male"      = "temp3", "adult_female"      = "temp4",
           "olderadult_male" = "temp5", "olderadult_female" = "temp6") %>%
    mutate(race = c(rep("black", 4), rep("hispanic", 4), rep("other", 4)),
           risk = rep(c("MSM", "IDU", "MSMandIDU", "other"), 3)) %>%
    gather(agegroup_gender, !!charVar, -race, -risk, na.rm = TRUE) %>%
    separate(agegroup_gender, c("agegroup", "gender")) %>%
    mutate(rwhap = as.integer(!!rwhap))
  )
}

read_combine_format_demo_breakdowns <-
  function(filepath, rwhap, sheet, categories, ranges) {
    #Aggregation function to help build the complete multiplier table needed for
    #correctly-leveled proportions

    #Get a list of read-in demographics matrices
    demoMats <- map2(categories, ranges, function(x,y)
      read_demo_matrix(filepath = filepath,
                       rwhap    = rwhap,
                       sheet    = sheet,
                       range    = y,
                       charVar  = x)) %>%
      `names<-`(categories)

    #Create a frame with all possible attribute combinations on to which the
    #data can be merged.
    base <- gen_demo_dist_base() %>%
      filter(!(gender == "female" & grepl("MSM", risk))) %>%
      mutate(rwhap = as.integer(!!rwhap))

    #Perform the join.  Information may be repeated in order to fit individuals
    #with a specific set of characteristics.
    for (i in names(demoMats)) {
      base <- left_join(base, demoMats[[i]],
                        by = c("agegroup", "gender", "race", "risk", "rwhap"))
    }

    #Verify that all rows have merged information.
    stopifnot(sum(is.na(base))==0)

    return(base)
  }

read_care_stage_breakdown <- function(filepath, rwhap) {
  #Helper function for calling and combining the demographics matrices for care
  #stages in particular.

  #Declare rwhap-specific sheet to be retrieved
  sheet <- assign_rwhap_sheet(rwhap, "Care Stage Breakdown")

  #Declare stages and ranges
  stages <- c("hiv",    "diag",   "care",   "suppress", "left")
  ranges <- c("C8:H19", "L8:Q19", "U8:Z19", "C26:H37",  "L26:Q37")

  #Retrieve
  read_combine_format_demo_breakdowns(filepath, rwhap, sheet, stages, ranges)
}

read_service_breakdown <- function(filepath, rwhap, suffix = NULL) {
  #Helper function for calling and combining the demographics matrices for
  #service need and receipt in particular.
  if(is.null(suffix)) suffix <- "Services Breakdown"

  #Declare rwhap-specific sheet to be retrieved
  sheet <- assign_rwhap_sheet(rwhap, suffix)

  #Declare services and ranges
  serviceVars <- c(               "oahsartreceipt",
                   "mcmneed",     "mcmreceipt",
                   "mhsaneed",    "mhsareceipt",
                   "supportneed", "supportreceipt")

  ranges      <- c(               "L8:Q19",
                   "C26:H37",     "L26:Q37",
                   "C44:H55",     "L44:Q55",
                   "C62:H73",     "L62:Q73")

  #Retrieve
  read_combine_format_demo_breakdowns(filepath, rwhap, sheet, serviceVars, ranges)
}

read_at_risk_breakdown <- function(filepath, rwhap) {
  #Declare sheet to be retrieved
  sheet <- "Transmission + At-Risk Features"

  #Call either rwhap or non-rwhap demo matrix
  if      (rwhap == TRUE)
    read_combine_format_demo_breakdowns(filepath, rwhap, sheet, "atrisk", "H8:M19")
  else if (rwhap == FALSE)
    read_combine_format_demo_breakdowns(filepath, rwhap, sheet, "atrisk", "H27:M38")
  else
    stop("'rwhap' must be of type logical.")
}

read_cell <- function(filepath, sheet, cell) {
  #Helper function. Reads a specific cell of an excel sheet but handles the name
  #conversion problems and brings it back as a much more flexible data format
  #(vector).
  suppressMessages(read_excel(filepath, sheet = sheet, range = cell, col_names = FALSE) %>%
    convert_unnamed() %>%
    pull("temp1"))
}

read_demo_breakdown <- function(filepath, brkdwn, sens_filepath = NULL, sens_tabName = NULL) {
  #High level function for calling any of the demographic matrix breakdowns with
  #a filepath and the breakdown desired (removing the need to declare sheet,
  #range, etc.
  brkdwn <- tolower(brkdwn)
  if        (brkdwn == "stage") {
    read_func <- read_care_stage_breakdown
  } else if (brkdwn == "service") {
    read_func <- read_service_breakdown
  } else if (brkdwn == "risk") {
    read_func <- read_at_risk_breakdown
  }

  #Conditional added for sensitivity analysis.  Only "else" clause used for
  #delivered application.
  if (!is.null(sens_filepath)){
    if (grepl("Need", sens_tabName)|grepl("Receipt", sens_tabName)){
      rwhapDist <- read_func(filepath = sens_filepath, rwhap = TRUE, suffix = sens_tabName)
      nonRwhapDist <- read_func(filepath = sens_filepath, rwhap = FALSE, suffix = sens_tabName)
    }
  }else{
    rwhapDist <- read_func(filepath = filepath, rwhap = TRUE)
    nonRwhapDist <- read_func(filepath = filepath, rwhap = FALSE)}

  #Combine data from rwhap and nonrwhap
  bind_rows(rwhapDist, nonRwhapDist) %>%
    arrange(agegroup, gender, race, risk, rwhap)
}

read_costs <- function(filepath, sens_costVal = NULL, sens_costValName = NULL) {
  #Function for reading the costs sheet of the input workbook.

  #Read qaly weights
  rawCd4Costs <- suppressMessages(read_excel(filepath, "Costs + Quality-of-Life Weights",
                            "A4:C13", col_names = FALSE) %>%
    convert_unnamed() %>%
    fill(temp1) %>%
    spread(temp1, temp3) %>%
    rename("cd4strata"     = "temp2",
           "edcost"        = "Emergency Department",
           "inpatientcost" = "Inpatient Department"))

  #Read costs
  rawServCosts <- suppressMessages(read_excel(filepath, "Costs + Quality-of-Life Weights",
                             "A14:C18", col_names = FALSE) %>%
    convert_unnamed() %>%
    spread(temp1, temp3) %>%
    rename("oahscost"    = "Outpatient Ambulatory Health Services (OAHS)",
           "artcost"     = "Antiretroviral Therapy (ART)",
           "mcmcost"     = "Medical Case Management (MCM)",
           "mhsacost"    = "Mental Health and/or Substance Abuse Services (MH/SA)",
           "supportcost" = "Support and Other Core Medical Services") %>%
    select(-temp2))

  if (!is.null(sens_costVal)) rawServCosts[sens_costValName] <- as.numeric(sens_costVal) *12

  #Combine
  list("rawcd4costs"  = rawCd4Costs,
       "rawservcosts" = rawServCosts)
}
