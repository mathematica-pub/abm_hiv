library(tidyverse)
library(assertr)
library(openxlsx)
library(ensurer)
library(gtools)
source(file.path(".", "modules", "post_processing", "supports.R"))

out_path <- file.path("./results")
scenario_nm_pattern <- "^(nrw|rw)_[0-9]{1,3}"
PARALLEL <- TRUE
yrs <- c(0:10)
subgrp_levels <- c("none", "agegroup", "gender", "race", "risk")
bootstrap_reps <- 1000

if (PARALLEL) {
  library(future.apply)
  library(furrr)
  plan(multisession, workers = 40, gc = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  lapply_func <- future_lapply
  map2_func <- future_map2
} else {
  lapply_func <- lapply
  map2_func <- map2
}

# Get the list of files for processing
scenario_filenames <- grep(scenario_nm_pattern, list.files(out_path), value = TRUE)
if (length(scenario_filenames) == 0) stop("No results to post-process")

scenario_names <-
  unique(gsub(paste0("(^", scenario_nm_pattern, ")_.*.rds$"), "\\1", scenario_filenames))

scenario_names_list <- map(
  scenario_names,
  ~ (scenario_filenames[grepl(paste0("^", .x, "_"), scenario_filenames)])
)
names(scenario_names_list) <- scenario_names

# Processing the saved files into a data format easier to manage.
calculated_scenarios <- lapply_func(names(scenario_names_list),
  read_and_extract_runs_data,
  scenario_names_list = scenario_names_list,
  out_path = out_path,
  PARALLEL = FALSE,
  yrs = yrs,
  subgrp_levels = subgrp_levels
) %>%
  `names<-`(names(scenario_names_list))

# Checkpoint because above steps can be glacial to redo
#saveRDS(calculated_scenarios, "calculated_scenarios.rds")

#calculated_scenarios <- readRDS("calculated_scenarios.rds")

# All scenarios in scenario_outcomes contain a single tibble
scenario_outcomes <- lapply(
  calculated_scenarios,
  extract_outcomes_to_bootstrap_by_subgrp
) %>%
  `names<-`(names(calculated_scenarios))

# Calculate within-scenario statistics
rw_scenario_names <- grep("^rw", names(scenario_outcomes), value = TRUE)
nrw_scenario_names <- grep("^nrw", names(scenario_outcomes), value = TRUE)
rw_outcomes <-
  calc_summary_stats_across_runs(rw_scenario_names,
    scenario_outcomes = scenario_outcomes
  ) %>%
  mutate(stat = paste(stat, "rw", sep = "_"))
nrw_outcomes <-
  calc_summary_stats_across_runs(nrw_scenario_names,
    scenario_outcomes = scenario_outcomes
  ) %>%
  mutate(stat = paste(stat, "nrw", sep = "_"))

# Calculate bootstrap for cross-scenario stats (e.g. icer)
# This part creates combinations but doesn't calculate, creates plan frame
bootstrap_plan <- tibble(
  rw = sample(rw_scenario_names, bootstrap_reps, replace = TRUE),
  nrw = sample(nrw_scenario_names, bootstrap_reps, replace = TRUE)
) %>%
  group_by(rw, nrw) %>%
  summarize(n = n(), .groups = "drop")

# Calculate diffs as needed to fill out bootstrap plan
bootstrapped_intermediate <- mutate(bootstrap_plan,
  diffs = map2(rw, nrw,
    calculate_scenario_outcome_diffs,
    scenario_outcomes = scenario_outcomes,
    yrs = yrs
  )
) %>%
  mutate(outcomes = map(diffs, reshape_diffs_wide_and_cleanup)) %>%
  select(-diffs) %>%
  unnest(outcomes)

# Reformat with summary statistics across distribution
bootstrapped_outcomes <- uncount(bootstrapped_intermediate, n) %>%
  select(-rw, -nrw) %>%
  map(custom_summary) %>%
  bind_rows(.id = "stat")

# Combined outputs
full_outcomes <- bind_rows(rw_outcomes, nrw_outcomes, bootstrapped_outcomes) %>%
  arrange(order(gtools::mixedorder(stat))) %>%
  filter(!is.na(q2.5)) %>%
  extract(
    stat,
    c("subgrp", "category", "stat", "year", "rwhap"),
    "^([a-z]*)_([a-zA-Z]*)_(.*)_year_([0-9]{1,2})_([nrw]{1,3})$"
  ) %>%
  mutate(
    year = as.integer(year),
    rwhap = as.integer(rwhap == "rw")
  )

# Output results
write.xlsx(full_outcomes, file = "equity_tables.xlsx")

