library(tidyverse)
library(assertr)
library(openxlsx)
source(file.path(".", "modules", "post_processing", "supports.R"))

out_path <- file.path("./results")
rw_data_list <-
  list(
    "simData" = read_rds(file.path(
      out_path,
      "rw_simData_2022-02-04_17_43_15.rds"
    )),
    "statsDFList" = read_rds(file.path(
      out_path,
      "rw_statsDFList_2022-02-04_17_43_15.rds"
    ))
  )

nrw_data_list <-
  list(
    "simData" = read_rds(file.path(
      out_path,
      "nrw_simData_2022-02-04_17_42_15.rds"
    )),
    "statsDFList" = read_rds(file.path(
      out_path,
      "nrw_statsDFList_2022-02-04_17_42_15.rds"
    ))
  )

yrs <- c(10)
subgrp_levels <- c("none", "agegroup", "gender", "race", "risk")
rw_tbl_list <-
  gather_scenario_equity_table_data(
    years = yrs,
    subgrp_levels = subgrp_levels,
    simData = rw_data_list$simData,
    statsDFList = rw_data_list$statsDFList
  )

nrw_tbl_list <-
  gather_scenario_equity_table_data(
    years = yrs,
    subgrp_levels = subgrp_levels,
    simData = nrw_data_list$simData,
    statsDFList = nrw_data_list$statsDFList
  )

comparison_tbls_list <-
  combine_rw_and_nrw_subgroup_tbls(rw_tbl_list, nrw_tbl_list)

write.xlsx(comparison_tbls_list, file = "equity_tables.xlsx")
