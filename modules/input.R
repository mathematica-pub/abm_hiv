
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
