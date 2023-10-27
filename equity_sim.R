library(gtools)
library(ensurer)
library(truncnorm)
library(assertthat)
library(assertr)
library(rlang)
library(readxl)
library(tidyverse)
library(tictoc)
for (fl in list.files("./modules/sim")) {
  print(fl)
  source(file.path(".", "modules", "sim", fl))
}

inputObj <- input_module(origin = "inputs_template.xlsx")
PREFIX <- "rw"
REPS <- 20
PARALLEL <- TRUE
savePath <- "."

if (PARALLEL) {
  library(future.apply)
  plan(multisession, workers = min(REPS, availableCores()-1), gc = TRUE)
  RNGkind("L'Ecuyer-CMRG")
}

run_sim <- function(inputObj,
                    prefix,
                    seed,
                    savePath = file.path(".")) {
  inputObj$testflag <- TRUE
  inputObj$valflag <- FALSE

  set.seed(seed)

  tic()
  simObj <- initialization_module(inputObj)
  toc()

  simData <- data.frame(list())
  statsDFList <- list(
    "deadStats" = data.frame(list()),
    "liveStats" = data.frame(list())
  )

  cs_r <- vector(mode = "double", length = simObj$duration)
  s_r <- vector(mode = "double", length = simObj$duration)

  for (i in 1:simObj$duration) {
    tic()
    print(i)

    simObj <- increment_module(simObj)
    simObj <- transmission_module(simObj)
    simObj <- care_stage_module(simObj)
    simObj <- health_state_module(simObj)
    simObj <- outcomes_module(simObj)
    simData <- bind_rows(simData, collapse_module(simObj))

      temp <- mutate(simObj$popdf, c_s = if_else(stage %in% c("care", "suppress"),
                                                 1, 0)) %>%
        pull(c_s) %>%
        table()
      cs_r[i] <- round(temp[2]/sum(temp)*100, 2)

      temp <- mutate(simObj$popdf, c_s = if_else(stage %in% c("suppress"),
                                                 1, 0)) %>%
        pull(c_s) %>%
        table()
      s_r[i] <- round(temp[2]/sum(temp)*100, 2)

    if (i==1 || i%%12==0) {
      print(paste("Care and Treatment rate:", cs_r[i]))
      print(paste("Suppression rate:", s_r[i]))
    }

    # manuscript 2 stats building
    statsDFList$simObj <- simObj
    statsDFList <- internal_cumulative_stats_module(statsDFList, i)
    simObj <- statsDFList$simObj
    toc()
  }

  simData <- inflate_module(simData, simObj$inflation)
  simDataDisc <- discount_module(simData, simObj$discount)

  simData <- list(
    notdisc = simData,
    disc = simDataDisc
  )

  timenow <- paste0(gsub("(:| )", "_", Sys.time()))

  saveRDS(
    statsDFList,
    file.path(savePath, paste0(prefix, "_statsDFList_", timenow, ".rds"))
  )
  saveRDS(
    simObj,
    file.path(savePath, paste0(prefix, "_simObj_", timenow, ".rds"))
  )
  saveRDS(
    simData,
    file.path(savePath, paste0(prefix, "_simData_", timenow, ".rds"))
  )

  return("DONE")
}

if (PARALLEL) {
  apply_func <- future_apply
} else {
  apply_func <- apply
}

run_definitions <- tibble(
  inputObj = rep(list(inputObj), REPS),
  prefix_num = paste(PREFIX, 21:(REPS+20), sep = "_"),
  seed = sample.int(1e6, REPS)
)

apply_func(run_definitions, 1, function(x) run_sim(x$inputObj, x$prefix_num, x$seed, savePath))

