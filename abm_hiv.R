library(gtools)
library(ensurer)
library(truncnorm)
library(assertthat)
library(rlang)
library(readxl)
library(tidyverse)
library(tictoc)
for (fl in list.files("./modules")) {
  print(fl)
  source(file.path(".", "modules", fl))
}

file_loc_input = "C:/Users/ravij/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/RWHAP_Equity-feat-add_equity_outcomes/inputs_2019/user_inputs_Current_RWHAP - 200K - PrEP.xlsx"

inputObj <- input_module(origin = file_loc_input)


inputObj$testflag <- TRUE
inputObj$valflag  <- FALSE

set.seed(inputObj$seed)

tic()
simObj   <- initialization_module(inputObj)
toc()

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

simData <- data.frame(list())
for (i in 1:simObj$duration) {
  tic()
  print(i)
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

trans_tree.df = bind_rows(trans_tree.df %>%
                            mutate(ID2 = as.character(ID2)),
                          simObj$trans_tree %>%
                            mutate(ID1 = as.character(ID1),
                                   ID2 = as.character(ID2)))

saveRDS(simData, "../results/rw_216months_7_26_22.rds")
saveRDS(simObj, "../results/rw_216months_7_26_22_simObj.rds")

#write_tsv(trans_tree.df,
#          file = "../results/rw_120months_7_11_22_trans_tree.tsv",
#          col_names = FALSE)

#write_tsv(simObj$diag_time,
#          file = "../results/rw_120months_7_11_22_diag_time.tsv",
#          col_names = FALSE)

# popdf_all = bind_rows(simObj$popdf_dead %>% select(id, gender, risk, age, race),
#                       simObj$popdf %>% select(id, gender, risk, age, race))
# diag_time = left_join(simObj$diag_time %>% filter(event == "diagnosis"),
#                       popdf_all,
#                       by = c("ID" = "id"))
# diag_time = diag_time %>%
#   rename(age_at_end = age)
#
# write_csv(diag_time,
#           file = "../results/SIM_diag_time_7_26_22.csv")
