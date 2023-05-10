
health_state_module <- function(simObj) {

  #Update cd4
  popHealth <- simObj$popdf %>%
    mutate(cd4 = if_else(stage == "suppress", precd4,
                         pmax(0, cd4 - !!simObj$cd4decrease)))

  #Update rna
  popHealth <- popHealth %>%
    mutate(rna = case_when(infectdur == 0          ~ "gt50k",
                           stage     == "suppress" ~ "lt200",
                           TRUE                    ~ as.character(rnaset)))

  #Update simulation object
  simObj$module <- "health_state"

  if (simObj$testflag) {
    stopifnot(nrow(simObj$popdf) == nrow(popHealth),
              all(select(simObj$popdf, -rna, -cd4) == select(popHealth, -rna,
                                                             -cd4),
                  na.rm = TRUE))
  }

  #Assign the new population dataset to the simObj so that it is carried forward
  #in the simulation.
  simObj$popdf <- popHealth

  #Return simulation object
  return(simObj)

}
