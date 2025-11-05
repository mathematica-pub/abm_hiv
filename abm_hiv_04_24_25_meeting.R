


file_loc_source = "./modules"
file_loc_link = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration_4_1_2025/Miami_demographics_20250325.csv"
file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Results/Incorrect_age/data_MSMincrease.xlsx"

library(gtools)
library(ensurer)
library(truncnorm)
library(assertthat)
library(rlang)
library(readxl)
library(tidyverse)
library(tictoc)
library(lubridate)
library(fastRG)
library(mice)

for (fl in list.files(file_loc_source)) {
  print(sprintf("%s", fl))
  source(paste(file_loc_source , fl, sep = "/"))
}

n_reps = 50
calibration_output_a = NULL

for (rep_sim in c(1:n_reps)) {

  Miamiflag   <- read_cell(file_loc_input, "High Level Pop + Sim Features", "E30")
  migrationflag   <- read_cell(file_loc_input, "High Level Pop + Sim Features", "E31")

  inputObj <- input_module(origin = file_loc_input)



  inputObj$testflag <- TRUE
  inputObj$valflag  <- FALSE

  #set.seed(inputObj$seed)

  print("Initializing population...")
  simObj   <- initialization_module(inputObj)

  simObj   <- initialize_prep(simObj,
                              origin = file_loc_input)

  simObj$notrans_tree.df <- tibble(ID1 = "None",
                                   ID2 = simObj$popdf$id,
                                   month = 0)

  simObj$trans_tree <- tibble(ID1 = integer(),
                              ID2 = integer(),
                              month = integer())

  simObj$diag_time <- tibble(ID = simObj$popdf %>%
                               filter(stage %in% c("suppress", "left", "diag", "care", "dead")) %>%
                               pull(id),
                             month = 0,
                             event = "initial",
                             cd4 = simObj$popdf %>%
                               filter(stage %in% c("suppress", "left", "diag", "care", "dead")) %>%
                               pull(cd4))

  simObj$popdf_dead = NULL
  simObj$popdf_migrate = NULL

  simData <- data.frame(list())

  if (!is.null(file_loc_link)) {
    link_county_abm.df = link_create(file_loc_link, simObj)

    #sprintf("Linkages...")
    #link_county_abm.df %>% as.data.frame() %>% print(quote = FALSE, row.names = FALSE)

  }

  print("Starting simulation...")
  print(paste("Month: ", "0", sep = ""))
  if (simObj$duration < 1) {
    simObj <- outcomes_module(simObj)
    simData <- bind_rows(simData, collapse_module(simObj))
  } else {
    for (i in 1:simObj$duration) {
      tic()
      print(paste("Month: ", i, sep = ""))
      simObj <- increment_module(simObj)
      simObj <- transmission_module(simObj)
      simObj <- care_stage_module(simObj)
      simObj <- health_state_module(simObj)
      simObj <- outcomes_module(simObj)
      simObj <- prep_update(simObj)
      if (migrationflag == TRUE) {
        simObj <- migration(simObj)
      }
      simData <- bind_rows(simData, collapse_module(simObj))
      toc()
    }
  }

  options(max.print = .Machine$integer.max)

  simData <- inflate_module(simData, simObj$inflation)
  simDataDisc <- discount_module(simData, simObj$discount)

  simData <- list(notdisc = simData,
                  disc    = simDataDisc)

  sprintf("Printing output...")

  sprintf("Calibration metrics...")

  calibration_output_risk = left_join(
    simObj$diag_time %>% filter(event == "diagnosis"),
    bind_rows(bind_rows(simObj$popdf %>% select(id, gender, risk, age, race, geo),
                        simObj$popdf_dead),
              simObj$popdf_migrate),
    by = join_by(ID == id)) %>%
    group_by(risk, month) %>%
    summarise(newinfects_agg = n()) %>%
    mutate(metric = "newinfects_agg",
           demographic = "risk") %>%
    rename("subgroup" = "risk",
           "stat" = "newinfects_agg") %>%
    select(metric, demographic, month, subgroup, stat) %>%
    mutate(subgroup = as.factor(subgroup))

  calibration_output_race = left_join(
    simObj$diag_time %>% filter(event == "diagnosis"),
    bind_rows(bind_rows(simObj$popdf %>% select(id, gender, risk, age, race, geo),
                        simObj$popdf_dead),
              simObj$popdf_migrate),
    by = join_by(ID == id)) %>%
    group_by(race, month) %>%
    summarise(newinfects_agg = n()) %>%
    mutate(metric = "newinfects_agg",
           demographic = "race") %>%
    rename("subgroup" = "race",
           "stat" = "newinfects_agg") %>%
    select(metric, demographic, month, subgroup, stat) %>%
    mutate(subgroup = as.factor(subgroup))

  calibration_output_geo = left_join(
    simObj$diag_time %>% filter(event == "diagnosis"),
    bind_rows(bind_rows(simObj$popdf %>% select(id, gender, risk, age, race, geo),
                        simObj$popdf_dead),
              simObj$popdf_migrate),
    by = join_by(ID == id)) %>%
    group_by(geo, month) %>%
    summarise(newinfects_agg = n()) %>%
    mutate(metric = "newinfects_agg",
           demographic = "geo") %>%
    rename("subgroup" = "geo",
           "stat" = "newinfects_agg") %>%
    select(metric, demographic, month, subgroup, stat) %>%
    mutate(subgroup = as.factor(subgroup))

  calibration_output = bind_rows(bind_rows(calibration_output_race,
                                           calibration_output_risk),
                                 calibration_output_geo)

  calibration_output = calibration_output %>%
    mutate(rep = rep_sim)

  calibration_output_a = bind_rows(calibration_output_a,
                                   calibration_output)

  print(c("#####################", rep_sim, "#################"))
}


calibration_output_yr = calibration_output_a  %>%
  mutate(year = trunc((month-1)/12)) %>%
  group_by(demographic, subgroup, year, rep) %>%
  summarize(total_year_sim = sum(stat))

calibration_output_yr_agg = calibration_output_yr  %>%
  group_by(demographic, subgroup, year) %>%
  summarize(avg_year_sim = mean(total_year_sim),
            sd_year_sim = sd(total_year_sim),
            r25_year_sim = quantile(total_year_sim, 0.25),
            r75_year_sim = quantile(total_year_sim, 0.75))

calibration_output_yr_agg$year = calibration_output_yr_agg$year + 2016

calibration_output_yr_agg_wide = calibration_output_yr_agg %>%
  select(demographic, subgroup, year, avg_year_sim) %>%
  pivot_wider(
    names_from = year,
    values_from = avg_year_sim)

write_csv(calibration_output_yr_agg_wide, "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Results/geo_avgs_2.csv")

