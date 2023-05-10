
collapse_module <- function(simObj) {
  # Condense to data that we need to store for the results.

  simObj$popdf <- mutate(simObj$popdf,
    newdeaths = stage == "dead",
    newinfects = infectmon == simObj$month,
    pospopsize = 1,
    ly = 1 / 12
  ) %>%
    group_by(rwhap, stage, agegroup, gender, race, risk) %>%
    summarize_at(
      .vars = vars(
        contains("cost"), "qaly", "newdeaths",
        "newinfects", "ly", "pospopsize"
      ),
      sum, na.rm = TRUE
    ) %>%
    ungroup() %>%
    mutate(
      month = simObj$month,
      pospopsize = if_else(stage == "dead", 0, pospopsize),
      ly = if_else(stage == "dead", 0, ly),
      popdenom = simObj$popdenom,
      negpopsize = n_distinct(setdiff(
        flatten_int(simObj$ids),
        simObj$popdf$id
      ))
    )

  if (simObj$testflag) {
    simObj$popdf <- ensure(
      simObj$popdf,
      nrow(distinct(
        ., rwhap, stage, agegroup,
        gender, race, risk
      )) == nrow(.)
    )
  }

  return(simObj$popdf)
}
