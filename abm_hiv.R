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

inputObj <- input_module(origin = "inputs_template.xlsx")

inputObj$testflag <- TRUE
inputObj$valflag  <- FALSE

set.seed(inputObj$seed)

tic()
simObj   <- initialization_module(inputObj)
toc()

simData <- data.frame(list())
for (i in 1:simObj$duration) {
  tic()
  print(i)
  simObj <- increment_module(simObj)
  simObj <- transmission_module(simObj)
  simObj <- care_stage_module(simObj)
  simObj <- health_state_module(simObj)
  simObj <- outcomes_module(simObj)
  simData <- bind_rows(simData, collapse_module(simObj))
  toc()
}

simData <- inflate_module(simData, simObj$inflation)
simDataDisc <- discount_module(simData, simObj$discount)

simData <- list(notdisc = simData,
                disc    = simDataDisc)
