

args <- commandArgs(trailingOnly = TRUE)
file_loc_source <- args[1]
file_loc_input <- args[2]

library(gtools)
library(ensurer)
library(truncnorm)
library(assertthat)
library(rlang)
library(readxl)
library(tidyverse)
library(tictoc)
for (fl in list.files(file_loc_source)) {
  print(sprintf("%s", fl))
  source(paste(file_loc_source , fl, sep = "/"))
}

inputObj <- input_module(origin = file_loc_input)


inputObj$testflag <- TRUE
inputObj$valflag  <- FALSE

set.seed(inputObj$seed)

print("Initializing population...")
simObj   <- initialization_module(inputObj)

simObj   <- initialize_prep(simObj,
                            origin = file_loc_input)

trans_tree.df <- tibble(ID1 = "None",
                        ID2 = simObj$popdf$id,
                        month = 0)

simObj$trans_tree <- tibble(ID1 = integer(),
                            ID2 = integer(),
                            month = integer())

simObj$diag_time <- tibble(ID = simObj$popdf %>%
                             filter(stage %in% c("suppress", "left", "diag", "care", "dead")) %>%
                             pull(id),
                           month = 0,
                           event = "initial")

simObj$popdf_dead = NULL

simData <- data.frame(list())

print("Starting simulation...")

for (i in 1:simObj$duration) {
  tic()
  print(sprintf("Month: %d", i))
  simObj <- increment_module(simObj)
  simObj <- transmission_module(simObj)
  simObj <- care_stage_module(simObj)
  simObj <- health_state_module(simObj)
  simObj <- outcomes_module(simObj)
  simObj <- prep_update(simObj)
  simData <- bind_rows(simData, collapse_module(simObj))
  toc()
}

simData <- inflate_module(simData, simObj$inflation)
simDataDisc <- discount_module(simData, simObj$discount)

simData <- list(notdisc = simData,
                disc    = simDataDisc)

sprintf("Printing output...")

sprintf("Calibration metrics...")

calibration_output <- simData$notdisc %>%
  group_by(risk, month) %>%
  summarise(newinfects_agg = sum(newinfects)) %>%
  pivot_longer(cols = c(newinfects_agg),
               names_to = "metric",
               values_to = "stat") %>%
  ungroup() %>%
  rename(subgroup = risk) %>%
  select(metric, month, subgroup, stat)


calibration_output %>% as.data.frame() %>% print(quote = FALSE, row.names = FALSE)

sprintf("Transmission tree...")

trans_tree.df = bind_rows(trans_tree.df %>%
                            mutate(ID2 = as.character(ID2)),
                          simObj$trans_tree %>%
                            mutate(ID1 = as.character(ID1),
                                   ID2 = as.character(ID2)))

trans_tree.df %>% as.data.frame() %>% print(quote = FALSE, row.names = FALSE)
#cat(format(as_tibble(trans_tree.df[c(1:5),]))[-c(1L,3L)], sep = "\n")

sprintf("Sequence sample times...")

simObj$diag_time %>% select(-cd4) %>%as.data.frame() %>% print(quote = FALSE, row.names = FALSE) #%>% print(n = Inf)
#cat(format(as_tibble(simObj$diag_time[c(1:5),]))[-c(1L,3L)], sep = "\n")

sprintf("PLWH demographics...")

simObj$popdf_dead %>% select(id, gender, risk, age, race) %>% as.data.frame() %>% print(quote = FALSE, row.names = FALSE)
simObj$popdf %>% select(id, gender, risk, age, race) %>% as.data.frame() %>% print(quote = FALSE, row.names = FALSE)



