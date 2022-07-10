
increment_module <- function(simObj) {

  #Clean up from last month as needed

  #Remove unneeded variables
  if (simObj$month!=0) {
    simObj$popdf <- simObj$popdf %>%
      select(-contains("cost"), -qaly)
  }

  #Remove dead people links from networks and from id lists
  HIVdead_ID = simObj$popdf %>%
    filter(stage == "dead") %>%
    pull(id)

  simObj$networks <- lapply(simObj$networks, filter, ID1 %!in% HIVdead_ID)
  simObj$ids      <- lapply(simObj$ids, function(x) x[!(x %in% HIVdead_ID)])

  #Remove dead people from population dataframe
  simObj$popdf <- simObj$popdf %>%
    filter(stage != "dead")

  #Update variables that change monthly.
  simObj$month <- simObj$month + 1
  simObj$popdf <- simObj$popdf %>%
    mutate(infectdur = infectdur + 1,
           age = age + 1/12) %>%
    gen_agegroup()
  simObj$popdenom <- simObj$popdenom * (1 + simObj$growthrate)

  return(simObj)
}
