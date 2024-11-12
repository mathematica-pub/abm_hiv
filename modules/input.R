
input_module <- function(origin) {
  #Controller function for grouping all of the input functions and
  #combining their output into one list.

  #---Gather Static Inputs---

  posPopSize  <- read_cell(origin, "High Level Pop + Sim Features",   "E8")
  negPopMult  <- read_cell(origin, "High Level Pop + Sim Features",   "E9")
  actPosSize  <- read_cell(origin, "High Level Pop + Sim Features",   "E10")
  usPopSize   <- read_cell(origin, "High Level Pop + Sim Features",   "E11")
  growthRate  <- (1 + read_cell(origin, "High Level Pop + Sim Features", "E12"))^(1/12) - 1 #Convert to monthly
  cd4decrease <- read_cell(origin, "CD4 + Viral Load Features",       "B14")
  duration    <- read_cell(origin, "High Level Pop + Sim Features",   "B5")*12 #Convert to months
  seed        <- read_cell(origin, "High Level Pop + Sim Features",   "B4")
  discount    <- read_cell(origin, "Costs + Quality-of-Life Weights", "F13")
  inflation   <- read_cell(origin, "Costs + Quality-of-Life Weights", "F14")

  if(Miamiflag == TRUE) {
  migration_in    <- read_cell(origin, "High Level Pop + Sim Features",   "E25")
  migration_out  <- read_cell(origin, "High Level Pop + Sim Features",   "E26")
  migration_within    <- read_cell(origin, "High Level Pop + Sim Features",   "E27")
  geography_all <- read_excel(origin, "Input_sheet", "A1:AW243")

  geography_all = geography_all %>%
    mutate(gender = case_when(
      Gender_groups == "Male" ~ "male",
      Gender_groups == "Female" ~ "female",
      .default = NA))

  geography_all = geography_all %>%
    mutate(agegroup = case_when(
      Age_groups == "Older Adult (55-100)" ~ "olderadult",
      Age_groups == "Adult (25-54)" ~ "adult",
      Age_groups == "Youth (13-24)" ~ "youth",
      .default = NA))

  geography_all = geography_all %>%
    mutate(risk = case_when(
      Risk_groups == "MSM (No IDU)" ~ "MSM",
      Risk_groups == "Other" ~ "other",
      Risk_groups == "MSM and IDU" ~ "MSMandIDU",
      Risk_groups == "IDU (No MSM)" ~ "IDU",
      Risk_groups == "No reported risk" ~ NA,
      .default = NA))

  geography_all = geography_all %>%
    mutate(race = case_when(
      Race_groups == "Other" ~ "other",
      Race_groups == "Black" ~ "black",
      Race_groups == "Hispanic" ~ "hispanic",
      .default = NA))

  geography_all = geography_all %>%
    mutate(stage = case_when(
      Stage == "Stage 2" ~ "hiv",
      Stage == "Stage 3" ~ "diag",
      Stage == "Stage 4" ~ "care",
      Stage == "Stage 5" ~ "suppress",
      Stage == "Stage 6" ~ "left",
      Stage == "Stage 7" ~ "dead",
      .default = NA))

  georeside <- geography_all %>%
    select(stage, agegroup, gender, race, risk,
           reside_R1:reside_R13)
  geomigrationin <- geography_all %>%
    select(stage, agegroup, gender, race, risk,
           migrate_in_R1:migrate_in_R13)
  geomigrationout <- geography_all %>%
    select(stage, agegroup, gender, race, risk,
           migrate_out_R1:migrate_out_R13)

  return(list("popprobs"          = list("hiv"     = suppressMessages(gen_hiv_demo_dist(origin)),
                                         "atrisk"  = suppressMessages(gen_at_risk_demo_dist(origin)),
                                         "service" = suppressMessages(gen_service_demo_dist(origin)),
                                         "rna"     = suppressMessages(gen_rna_dist(origin)),
                                         "nodes"   = suppressMessages(gen_rwhap_by_stage_dist(origin))),
              "costsandqaly"      = suppressMessages(gen_costs_qaly(origin)),
              "popcd4dists"       = suppressMessages(gen_cd4_defs(origin)),
              "origin"            = origin,
              "pospopulationsize" = posPopSize,
              "negpopulationmult" = negPopMult,
              "actualpospopsize"  = actPosSize,
              "uspopulation"      = usPopSize,
              "growthrate"        = growthRate, #This is a monthly rate at this point
              "cd4decrease"       = cd4decrease,
              "stagetransprobs"   = suppressMessages(gen_stage_transition_probs(origin)),
              "servtransmods"     = suppressMessages(gen_serv_transition_mods(origin)),
              "trans_params"      = suppressMessages(gen_trans_params(origin)),
              "duration"          = duration,
              "seed"              = seed,
              "discount"          = discount,
              "inflation"         = inflation,
              "migrationin"       = migration_in,
              "migrationout"      = migration_out,
              "migrationwithin"      = migration_within,
              "georeside"         = georeside,
              "geomigrationin"    = geomigrationin,
              "geomigrationout"   = geomigrationout))
  } else {
    return(list("popprobs"          = list("hiv"     = suppressMessages(gen_hiv_demo_dist(origin)),
                                           "atrisk"  = suppressMessages(gen_at_risk_demo_dist(origin)),
                                           "service" = suppressMessages(gen_service_demo_dist(origin)),
                                           "rna"     = suppressMessages(gen_rna_dist(origin)),
                                           "nodes"   = suppressMessages(gen_rwhap_by_stage_dist(origin))),
                "costsandqaly"      = suppressMessages(gen_costs_qaly(origin)),
                "popcd4dists"       = suppressMessages(gen_cd4_defs(origin)),
                "origin"            = origin,
                "pospopulationsize" = posPopSize,
                "negpopulationmult" = negPopMult,
                "actualpospopsize"  = actPosSize,
                "uspopulation"      = usPopSize,
                "growthrate"        = growthRate, #This is a monthly rate at this point
                "cd4decrease"       = cd4decrease,
                "stagetransprobs"   = suppressMessages(gen_stage_transition_probs(origin)),
                "servtransmods"     = suppressMessages(gen_serv_transition_mods(origin)),
                "trans_params"      = suppressMessages(gen_trans_params(origin)),
                "duration"          = duration,
                "seed"              = seed,
                "discount"          = discount,
                "inflation"         = inflation))
  }
}


