
outcomes_module <- function(simObj) {
  #Generate variables that contain the cost that each individual incurs in
  #each category.

  #Convert actual cd4 of an individual into the required cd4strata for merging
  #costs, then merge costs.
  popDf <- simObj$popdf %>%
    mutate(cd4strata = case_when(             cd4 <  51  ~ "<=50",
                                 51  <= cd4 & cd4 <  201 ~ "51-200",
                                 201 <= cd4 & cd4 <  351 ~ "201-350",
                                 351 <= cd4 & cd4 <= 500 ~ "351-500",
                                 500 <  cd4              ~ ">500")) %>%
    left_join(simObj$costsandqaly$cd4costs, by = c("stage", "cd4strata")) %>%
    left_join(simObj$costsandqaly$servcosts,
              by = c("stage", "mcm", "mhsa", "support"))

  if (simObj$testflag) popDf <- ensure(popDf,
                                       all(!is.na(select(., contains("cost")))))

  #Convert actual cd4 of an individual into the required cd4strata for merging
  #qalys, then merge qalys.
  popDf <- popDf %>%
    mutate(cd4strata = case_when(             cd4 < 100 ~ "<100",
                                 100 <= cd4 & cd4 < 200 ~ "100-199",
                                 200 <= cd4 & cd4 < 350 ~ "200-349",
                                 350 <= cd4 & cd4 < 500 ~ "350-499",
                                 500 <= cd4             ~ "500+")) %>%
    left_join(simObj$costsandqaly$qaly, by = c("stage", "cd4strata")) %>%
    select(-cd4strata)

  if (simObj$testflag) popDf <- ensure(popDf, all(!is.na(.$qaly)))

  #Update simulation object
  simObj$module <- "outcome"

  if (simObj$testflag) {
    stopifnot(nrow(simObj$popdf) == nrow(popDf),
              all(select(simObj$popdf, -contains("cost"), -contains("qaly")) ==
                    select(popDf, -contains("cost"), -contains("qaly")),
                  na.rm = TRUE))
  }

  #Assign the new population dataset to the simobj so that it is carried forward
  #in the simulation.
  simObj$popdf <- popDf

  #Return simulation object
  return(simObj)
}
