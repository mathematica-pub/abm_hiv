internal_stats_module <- function(dflist, month) {
  # To gather point in time for a single month
  varnms <- c(
    "ever_4", "ever_5", "ever_6", "always_45", "always_456", "always_5",
    "ever_always_5", "ever_always_45", "ever_always_456", "ever_month_45"
  )
  if (month == 1) { # Only in first month of simulation
    dflist$simObj$popdf <- left_join(
      dflist$simObj$popdf,
      as_tibble(t(rep(FALSE, length(varnms)) %>%
        `names<-`(varnms))),
      by = character()
    )
  }

  # If last month of the year, create and extract data.
  # NOT THE SAME AS MANUSCRIPT 2 - Year end not over year
  if ((month %% 12 == 0) | month == 1) {
    dflist$simObj$popdf <- dflist$simObj$popdf %>%
      mutate(
        ever_4 = if_else(stage == "care", TRUE, FALSE),
        ever_5 = if_else(stage == "suppress", TRUE, FALSE),
        ever_6 = if_else(stage == "left", TRUE, FALSE),
        always_5 = ever_5,
        always_45 = ever_4 | ever_5,
        always_456 = ever_4 | ever_5 | ever_6
      )
    # Remove the dead since otherwise they will be lost when the increment module is run.
    dflist$deadStatsYr <- filter(dflist$simObj$popdf, stage == "dead")

    # Record the records that correspond to data for this year.
    yr <- floor(month / 12)
    dflist$simObj$popdf <- ever_outcomes_calc(dflist$simObj$popdf)
    dflist$deadStats <- mutate(
      dflist$deadStatsYr,
      year = yr,
      popdenom = dflist$simObj$popdenom,
      newinfects = if_else(
        is.na(infectmon),
        FALSE,
        infectmon == dflist$simObj$month
      )
    ) %>%
      ever_outcomes_calc() %>%
      bind_rows(dflist$deadStats)
    dflist$liveStats <- dflist$simObj$popdf %>%
      filter(stage != "dead") %>%
      mutate(
        year = yr,
        popdenom = dflist$simObj$popdenom,
        newinfects = if_else(
          is.na(infectmon),
          FALSE,
          infectmon == dflist$simObj$month
        )
      ) %>%
      bind_rows(dflist$liveStats)
  }

  return(dflist)
}

internal_cumulative_stats_module <- function(dflist, month) {
  # To gather cumulative stats by year and at the end of month 1.
  varnms <- c(
    "ever_4", "ever_5", "ever_6", "always_45", "always_456", "always_5",
    "ever_always_5", "ever_always_45", "ever_always_456", "ever_month_45"
  )
  if (month == 1) { # Only in first month of simulation
    dflist$simObj$popdf <- left_join(
      dflist$simObj$popdf,
      as_tibble(t(rep(FALSE, length(varnms)) %>%
        `names<-`(varnms))),
      by = character()
    )
  } else { # All other months initialize for new infects.
    dflist$simObj$popdf <- mutate(
      dflist$simObj$popdf,
      across(all_of(varnms), ~ if_else(infectdur == 0 & is.na(.x), FALSE, .x))
    )
  }

  # Same as manuscript 2
  if (month %% 12 == 1) { # First month of any year
    dflist$simObj$popdf <- dflist$simObj$popdf %>%
      mutate(
        ever_4 = if_else(stage == "care", TRUE, FALSE),
        ever_5 = if_else(stage == "suppress", TRUE, FALSE),
        ever_6 = if_else(stage == "left", TRUE, FALSE),
        always_5 = ever_5,
        always_45 = ever_4 | ever_5,
        always_456 = ever_4 | ever_5 | ever_6
      )
  } else { # All other months
    dflist$simObj$popdf <- dflist$simObj$popdf %>%
      mutate(
        always_5 = case_when(
          stage == "dead" ~ always_5,
          stage != "suppress" ~ FALSE,
          !ever_5 & !ever_6 & stage == "suppress" ~ TRUE,
          TRUE ~ always_5
        ),
        always_45 = case_when(
          stage == "dead" ~ always_45,
          stage != "care" & stage != "suppress" ~ FALSE,
          !ever_4 & !ever_5 & !ever_6 & stage == "care" ~ TRUE,
          TRUE ~ always_45
        ),
        always_456 = case_when(
          stage == "dead" ~ always_456,
          stage != "care" & stage != "suppress" & stage != "left" ~ FALSE,
          !ever_4 & !ever_5 & !ever_6 & stage == "care" ~ TRUE,
          TRUE ~ always_456
        ),
        ever_4 = if_else(stage == "care", TRUE, ever_4),
        ever_5 = if_else(stage == "suppress", TRUE, ever_5),
        ever_6 = if_else(stage == "left", TRUE, ever_6)
      )
  }

  # Remove the dead since otherwise they will be lost when the increment module is run.
  deadStatsAdd <- filter(dflist$simObj$popdf, stage == "dead")

  if (month %% 12 == 1) {
    dflist$deadStatsYr <- deadStatsAdd
  } else {
    dflist$deadStatsYr <- bind_rows(deadStatsAdd, dflist$deadStatsYr)
  }

  # Record the records that correspond to data for this year.
  if ((month %% 12 == 0) | month == 1) { # Also run this if last month of any year
    yr <- floor(month / 12)
    dflist$simObj$popdf <- ever_outcomes_calc(dflist$simObj$popdf)
    dflist$deadStats <- mutate(
      dflist$deadStatsYr,
      year = yr,
      popdenom = dflist$simObj$popdenom,
      newinfects = if_else(is.na(infectmon), FALSE,
        infectmon > (dflist$simObj$month - 12)
      )
    ) %>%
      ever_outcomes_calc() %>%
      bind_rows(dflist$deadStats)
    dflist$liveStats <- dflist$simObj$popdf %>%
      filter(stage != "dead") %>%
      mutate(
        year = yr,
        popdenom = dflist$simObj$popdenom,
        newinfects = if_else(is.na(infectmon), FALSE,
          infectmon > (dflist$simObj$month - 12)
        )
      ) %>%
      bind_rows(dflist$liveStats)
  }

  return(dflist)
}

ever_outcomes_calc <- function(data) {
  mutate(data,
    ever_always_5   = if_else(always_5, TRUE, ever_always_5),
    ever_always_45  = if_else(always_45, TRUE, ever_always_45),
    ever_always_456 = if_else(always_456, TRUE, ever_always_456),
    ever_month_45   = if_else(ever_4 | ever_5, TRUE, ever_month_45)
  )
}


collapse_year_manu2 <- function(data) {
  data %>%
    select_at(c(
      "year", "newinfects", "ever_4", "ever_5", "ever_6", "always_45", "always_456",
      "always_5", "ever_always_5", "ever_always_45", "ever_always_456",
      "ever_month_45", "popdenom"
    )) %>%
    mutate(
      n = 1,
      pct_ever_care_num = ever_4 | ever_5
    ) %>%
    group_by(year) %>%
    mutate(popdenom = max(popdenom)) %>%
    group_by(year, popdenom) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    ensure(
      all(.$ever_always_5 <= .$ever_always_45),
      all(.$ever_always_45 <= .$ever_always_456),
      all(.$ever_always_45 <= .$ever_month_45),
      all(.$ever_4 <= .$ever_month_45),
      all(.$ever_5 <= .$ever_month_45),
      all(.$always_5 <= .$ever_5),
      all(.$always_45 <= (.$ever_4 + .$ever_5)),
      all(.$always_456 <= (.$ever_4 + .$ever_5 + .$ever_6)),
      all(.$always_5 <= .$always_45),
      all(.$always_45 <= .$always_456),
      all(.$always_456 <= .$n)
    ) %>%
    mutate(
      pct_ever_care_denom = n,
      pct_suppress_denom = n
    ) %>%
    rename(
      pct_suppress_num = always_5,
      pct_retain_num = always_45,
      pct_retain_denom = always_456
    )
}

calculate_main_manu2_table <- function(deadStats, liveStats) {
  # Calculate yearly items
  deadStats <- ensure(deadStats, n_distinct(deadStats$id) == nrow(deadStats))
  deadStatsYr <- deadStats %>%
    collapse_year_manu2()
  liveStatsYr <- liveStats %>%
    collapse_year_manu2()
  yearRows <- bind_rows(deadStatsYr, liveStatsYr) %>%
    group_by(year, popdenom) %>%
    summarize_at(vars(-group_cols()), sum) %>%
    ungroup() %>%
    mutate(
      pct_suppress = pct_suppress_num / pct_suppress_denom,
      pct_retain = pct_retain_num / pct_retain_denom,
      pct_ever_care = pct_ever_care_num / pct_ever_care_denom,
      incidence = newinfects / popdenom * 100000,
      prevalence = n / popdenom * 100000,
      year = as.character(year)
    ) %>%
    select(
      year,
      pct_suppress_num, pct_suppress_denom, pct_suppress,
      pct_retain_num, pct_retain_denom, pct_retain,
      pct_ever_care_num, pct_ever_care_denom, pct_ever_care,
      newinfects, incidence, n, prevalence, popdenom
    )

  # Calculate ever items
  deadStatsEver <- deadStatsYr %>%
    mutate(popdenom = 0) %>%
    select(-year) %>%
    summarize_all(sum) %>%
    ungroup()
  liveStatsEver <- liveStatsYr %>%
    filter(year == max(year))
  everRow <- bind_rows(deadStatsEver, liveStatsEver) %>%
    summarize_all(sum) %>%
    mutate(
      pct_suppress = ever_always_5 / pct_suppress_denom,
      pct_retain = ever_always_45 / ever_always_456,
      pct_ever_care = ever_month_45 / pct_ever_care_denom,
      year = "Ever",
      pct_suppress_num = ever_always_5,
      pct_retain_num = ever_always_45,
      pct_ever_care_num = ever_month_45,
      pct_retain_denom = ever_always_456
    ) %>%
    select(
      year,
      pct_suppress_num, pct_suppress_denom, pct_suppress,
      pct_retain_num, pct_retain_denom, pct_retain,
      pct_ever_care_num, pct_ever_care_denom, pct_ever_care
    )

  # Combine and output
  bind_rows(yearRows, everRow)
}

