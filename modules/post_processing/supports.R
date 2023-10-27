calc_simData_type_time_range_vars <-
  function(simDataType, grp_vars, start_month, end_month) {
    simData_yr <- mutate(simDataType, totalcost = rowSums(across(contains("cost")))) %>%
      filter(month >= start_month, month <= end_month) %>% # cumulative over year
      mutate(popdenom = max(if_else(month == end_month, popdenom, 0, 0))) %>%
      group_by(!!!syms(grp_vars)) %>%
      mutate(pospopsize = sum(if_else(month == end_month, pospopsize, 0, 0))) %>%
      summarize(
        popdenom = last(popdenom),
        pospopsize = last(pospopsize),
        across(c(contains("cost"), qaly, newinfects, newdeaths), sum),
        .groups = "drop"
      ) %>%
      select(!!!grp_vars, pospopsize, totalcost, qaly, newinfects, newdeaths, popdenom)

    neg_pop_qaly_val <- filter(
      simDataType,
      month >= start_month, month <= end_month
    ) %>%
      group_by(month) %>%
      summarize(negpopqaly = negpopsize[1] / 12, .groups = "drop") %>%
      summarize(negpopqaly = sum(negpopqaly)) %>%
      pull(negpopqaly)

    simData_mon <- mutate(simDataType, totalcost = rowSums(across(contains("cost")))) %>%
      filter(month >= end_month, month <= end_month) %>%
      group_by(stage) %>%
      summarize(n = sum(pospopsize)) %>%
      mutate(pct = n/sum(n) * 100) %>%
      filter(stage %in% c("care", "suppress", "left")) %>%
      select(-n) %>%
      pivot_wider(names_from = stage, values_from = pct)

    simData_yr <- mutate(simData_yr, negpopqaly = neg_pop_qaly_val) %>%
      bind_cols(simData_mon)

    return(simData_yr)
  }

calc_cumulative_to_yr <-
  function(yr, simData, statsDFList,
           grp_vars = c("rwhap", "agegroup", "gender", "race", "risk")) {

    # if (yr == 0) {
    #   out_df <- mon_0_stats_module(simInit, grp_vars)
    # } else {
    if (yr == 0) {
      start_month <- 1
      end_month <- 1
    } else {
      start_month <- (yr * 12) - 11
      end_month <- yr * 12
    }

    # discounted is model to date
    disc_cum <- calc_simData_type_time_range_vars(
      simData$disc,
      grp_vars, 1, end_month
    ) %>%
      select(!!!grp_vars,
        pospopsize_disc = pospopsize,
        totalcost_disc = totalcost,
        qaly_disc = qaly,
        negpopqaly_disc = negpopqaly,
        newinfects_disc = newinfects,
        newdeaths_disc = newdeaths,
        popdenom_disc = popdenom
      )

    # undiscounted is cumulative within a year EXCEPT stage percentages
    notdisc_yr <- calc_simData_type_time_range_vars(
      simData$notdisc,
      grp_vars, start_month, end_month
    )

    # Use other stats to get "ever_stage" information and other variables for
    # verification purposes
    living_yr <- mutate(statsDFList$liveStats,
      ever_4_or_5 = ever_4 | ever_5
    ) %>%
      filter(year == yr) %>%
      group_by(!!!syms(grp_vars)) %>%
      summarize(
        popdenom = max(popdenom), # Needs same logic as above if discounting
        pospopsize = n(),
        across(contains("ever"), sum),
        snap_care = sum(stage=="care"),
        snap_suppress = sum(stage=="suppress"),
        snap_care_suppress = sum(stage %in% c("care", "suppress")),
        .groups = "drop"
      ) %>%
      select(
        !!!syms(grp_vars), pospopsize, ever_4, ever_5, ever_4_or_5, ever_6,
        popdenom, snap_care, snap_suppress, snap_care_suppress
      )

    stopifnot(
      sum(notdisc_yr$pospopsize) == sum(living_yr$pospopsize),
      unique(notdisc_yr$popdenom) == unique(living_yr$popdenom)
    )

    dead_yr <- mutate(statsDFList$deadStats,
      ever_4_or_5 = ever_4 | ever_5
    ) %>%
      filter(year == yr) %>%
      group_by(!!!syms(grp_vars)) %>%
      summarize(
        popdenom = max(popdenom),
        newdeaths = n(),
        across(contains("ever"), sum),
        .groups = "drop"
      ) %>%
      select(
        !!!grp_vars, newdeaths,
        ever_4, ever_5, ever_4_or_5, ever_6, popdenom
      )

    stopifnot(
      sum(notdisc_yr$newdeaths) == sum(dead_yr$newdeaths),
      unique(notdisc_yr$popdenom) == max(dead_yr$popdenom)
    )

    # Combine the living and dead trackers to get all "ever_stage" information
    # for merger
    living_and_dead_yr <- bind_rows(living_yr, dead_yr) %>%
      group_by(!!!syms(grp_vars)) %>%
      summarize(
        popdenom = last(popdenom),
        across(-popdenom, function(x) sum(x, na.rm = TRUE)),
        .groups = "drop"
      )

    out_yr <- left_join(disc_cum, notdisc_yr, by = grp_vars) %>%
      left_join(
        select(living_and_dead_yr, !!!grp_vars, contains("ever"), contains("snap")),
        by = grp_vars
      )

    # Add year variable in preparation for stacking and use with other year
    # calculations.
    out_df <- mutate(out_yr, year = yr) %>%
      select(year, everything())

    # }

    return(out_df)
  }

gather_scenario_raw_equity_data <-
  function(years, simData, statsDFList, grp_vars) {
    lapply(years,
      calc_cumulative_to_yr,
      simData = simData,
      statsDFList = statsDFList,
      grp_vars = grp_vars
    ) %>%
      bind_rows() %>%
      verify(!is.na(.))
  }

roll_up_raw_scenario_equity_to_subgrp <- function(subgrp, years_data, grp_vars) {
  other_grp_vars <- grp_vars[!(subgrp == grp_vars)]
  raw_df <- select(years_data, -any_of(other_grp_vars))
  if (subgrp != "none") {
    grped_df <- group_by(raw_df, !!sym(subgrp), year)
  } else {
    grped_df <- group_by(raw_df, year)
  }
  out_df <- summarize(grped_df,
    popdenom = first(popdenom),
    popdenom_disc = first(popdenom_disc),
    negpopqaly = first(negpopqaly),
    negpopqaly_disc = first(negpopqaly_disc),
    care = first(care),
    suppress = first(suppress),
    left = first(left),
    across(c(-popdenom, -popdenom_disc, -negpopqaly, -negpopqaly_disc, -care,
             -suppress, -left),
           sum),
    .groups = "drop"
  ) %>%
    arrange(year)
  return(out_df)
}

gather_scenario_equity_table_data <-
  function(years,
           subgrp_levels,
           simData,
           statsDFList,
           grp_vars = c("rwhap", "agegroup", "gender", "race", "risk")) {

    years_data <-
      gather_scenario_raw_equity_data(years, simData, statsDFList, grp_vars)

    data_list_by_subgroup <- lapply(subgrp_levels,
      roll_up_raw_scenario_equity_to_subgrp,
      years_data = years_data,
      grp_vars = grp_vars
    ) %>%
      `names<-`(subgrp_levels)

    ratio_outcomes <- c("newinfects", "ever_6", "ever_4_or_5", "ever_5", "qaly",
                        "snap_care", "snap_suppress", "snap_care_suppress")
    data_list_by_subgroup_w_ratios_indices <-
      lapply(names(data_list_by_subgroup),
        calculate_subgrp_ratios_and_indices,
        tbl_data_list = data_list_by_subgroup,
        outcomes = ratio_outcomes
      ) %>%
      `names<-`(names(data_list_by_subgroup))

    # update qalys just for the "none" specification -> qaly is 0 if dead at
    # end of year and measuring end of year
    if ("none" %in% subgrp_levels) {
      data_list_by_subgroup_w_ratios_indices$none <-
        mutate(data_list_by_subgroup_w_ratios_indices$none,
          qaly_disc = qaly_disc + negpopqaly_disc,
          qaly = qaly + negpopqaly
        )
    }

    transposed_data_by_subgroup <-
      lapply(names(data_list_by_subgroup_w_ratios_indices),
        transpose_and_filter_scenario_equity_table_data,
        tbl_data_list = data_list_by_subgroup_w_ratios_indices,
        outcomes = ratio_outcomes
      ) %>%
      `names<-`(names(data_list_by_subgroup_w_ratios_indices))

    return(transposed_data_by_subgroup)
  }

combine_rw_and_nrw_subgroup_tbls <- function(rw_tbl_list, nrw_tbl_list) {
  stopifnot(all(names(rw_tbl_list) == names(nrw_tbl_list)))

  map2(
    rw_tbl_list, nrw_tbl_list,
    function(rw, nrw) {
      bind_rows(
        mutate(rw, rwhap = "with_rwhap"),
        mutate(nrw, rwhap = "without_rwhap")
      ) %>%
        pivot_longer(contains("year")) %>%
        unite(name, c(name, rwhap)) %>%
        pivot_wider()
    }
  ) %>%
    `names<-`(names(rw_tbl_list))
}

calculate_subgrp_ratios_and_indices <- function(subgrp, tbl_data_list, outcomes) {
  tbl_data <- tbl_data_list[[subgrp]]
  stats <- c("prop_tot", "prop", "abs_diff", "rel_diff", "ti", "bgv", "idis",
             "sg_pct")
  if (subgrp == "none") {
    varnms <- crossing(outcome = outcomes, stat = stats) %>%
      mutate(
        stat = factor(stat, levels = stats, ordered = TRUE),
        outcome = factor(outcome, levels = outcomes, ordered = TRUE)
      ) %>%
      arrange(outcome, stat) %>%
      unite(stat, everything()) %>%
      pull(stat)
    out_df <- cross_join(
      tbl_data,
      as_tibble(t(rep(NA_real_, length(varnms)) %>%
        `names<-`(varnms)))
    ) %>%
      group_by(year) %>%
      mutate(tot_pospop = sum(pospopsize) + sum(newdeaths),
             ever_4_or_5_pct = ever_4_or_5 / tot_pospop,
             ever_5_pct = ever_5 / tot_pospop,
             snap_care_pct = snap_care/tot_pospop,
             snap_care_suppress_pct = snap_care_suppress/tot_pospop,
             snap_suppress_pct = snap_suppress/tot_pospop) %>%
      ungroup() %>%
      select(-tot_pospop)
  } else {
    reference_groups <- list(
      "race" = "other",
      "gender" = "male",
      "agegroup" = "olderadult",
      "risk" = "MSM"
    )
    out_df <- add_outcome_metrics(tbl_data, subgrp, outcomes, reference_groups)
  }

  return(out_df)
}

add_outcome_metrics <- function(tbl_data, subgrp, outcomes, reference_groups) {
  out_df <- group_by(tbl_data, year) %>%
    mutate(
      tot_pospop = sum(pospopsize) + sum(newdeaths),
      prop_pospop = (pospopsize + newdeaths) / tot_pospop,
      subgrp_pospop = (pospopsize + newdeaths),
      n = n_distinct(!!sym(subgrp))
    )
  for (outcome in outcomes) {
    # Table 1 proportion
    # qaly_pp is the output for table 1
    prop_tot <- paste0(outcome, "_prop_tot")
    sg_pct <- paste0(outcome, "_sg_pct") #New Table 2A, 2B
    out_df <- mutate(
      out_df,
      !!sym(prop_tot) := !!sym(outcome) / tot_pospop,
      !!sym(sg_pct) := !!sym(outcome) / subgrp_pospop
    )

    # Table 2 rate ratios
    prop <- paste0(outcome, "_prop")
    abs <- paste0(outcome, "_abs_diff")
    rel <- paste0(outcome, "_rel_diff")
    out_df <- mutate(
      out_df,
      !!sym(prop) := !!sym(outcome) / subgrp_pospop * 100000,
      ref_outcome = max(
        if_else(
          !!sym(subgrp) == reference_groups[[subgrp]],
          !!sym(prop),
          0
        )
      ),
      !!sym(abs) := !!sym(prop) - ref_outcome,
      !!sym(rel) := !!sym(prop) / ref_outcome
    ) %>%
      select(-ref_outcome)

    # Table 3 indexes
    ti <- paste0(outcome, "_ti")
    bgv <- paste0(outcome, "_bgv")
    idis <- paste0(outcome, "_idis")
    out_df <- mutate(
      out_df,
      mean_outcome = sum(!!sym(outcome)) / tot_pospop * 100000,
      ti_part = prop_pospop *
        (!!sym(prop) / mean_outcome) *
        log(!!sym(prop) / mean_outcome),
      !!sym(ti) := abs(sum(ti_part)),
      bgv_part = prop_pospop * ((!!sym(prop) - mean_outcome)^2),
      !!sym(bgv) := sum(bgv_part),
      idis_part = abs(!!sym(prop) - mean_outcome),
      !!sym(idis) := (1 / n) * (sum(idis_part) / mean_outcome)
    ) %>%
      verify(n_distinct(n) == 1)

    # loop clean up
    out_df <- select(out_df, -mean_outcome, -ti_part, -bgv_part, -idis_part)
  }

  # outer clean up
  out_df <- select(out_df, -tot_pospop, -prop_pospop, -subgrp_pospop, -n)

  return(out_df)
}

transpose_and_filter_scenario_equity_table_data <-
  function(subgrp, tbl_data_list, outcomes) {
    tbl_data <- tbl_data_list[[subgrp]]

    if (subgrp == "none") {
      long_data <- pivot_longer(tbl_data, -year)
    } else {
      long_data <- pivot_longer(tbl_data, c(-year, -!!subgrp))
    }

    stat_levels <- lapply(outcomes, grep, x = colnames(tbl_data), value = TRUE) %>%
      flatten_chr() %>%
      unique() #Needed due to snap_care_* construction
    stat_levels <- c(
      "popdenom", "pospopsize", "newdeaths", "ever_4", "newdeaths_disc",
      "totalcost_disc", "care", "suppress", "left", "pct_suppress", "pct_retain",
      "pct_ever_care", stat_levels
    )

    transposed_tbl_data <- mutate(long_data, year = paste0("year_", year)) %>%
      pivot_wider(names_from = year) %>%
      mutate(stat = factor(name, levels = stat_levels, ordered = TRUE)) %>%
      arrange(stat) %>%
      select(stat, everything(), -name)

    if (subgrp != "none") {
      transposed_tbl_data <- group_by(transposed_tbl_data, stat) %>%
        nest() %>%
        mutate(
          data := map(
            data,
            function(x) {
              out_data <- mutate(x,
                num_vals = select(x, -!!subgrp) %>%
                  apply(2, n_distinct) %>%
                  max(),
                !!sym(subgrp) := if_else(num_vals == 1, "all", !!sym(subgrp))
              ) %>%
                select(-num_vals)
            }
          )
        ) %>%
        unnest(data) %>%
        distinct()
    }

    return(filter(transposed_tbl_data, !is.na(stat)))
  }

read_valid_runs_scenario <- function(scenario_name,
                                     scenario_names_list,
                                     out_path,
                                     PARALLEL) {
  if (PARALLEL) {
    lapply_func <- future_lapply
  } else {
    lapply_func <- lapply
  }

  filenames <- scenario_names_list[[scenario_name]]
  filenames <- filenames[grepl(paste0(
    "^", scenario_name, "_(simData|statsDFList)_"
  ), filenames)]

  loadedruns <- lapply_func(
    filenames,
    function(x) {
      tryCatch(
        readRDS(file.path(out_path, x)),
        error = function(c) {
          print(x)
        }
      )
    }
  ) %>%
    `names<-`(gsub(
      paste0("^(", scenario_name, "_(simData|statsDFList)).*"),
      "\\1", filenames
    ))

  # separate into different filetypes
  loadedScenarios <- list(
    simData = flatten(
      loadedruns[grep(paste0("^", scenario_name, "_simData"),
        names(loadedruns),
        value = TRUE
      )]
    ),
    statsDFList = flatten(
      loadedruns[grep(paste0("^", scenario_name, "_statsDFList"),
        names(loadedruns),
        value = TRUE
      )]
    )
  )

  return(loadedScenarios)
}

read_and_extract_runs_data <- function(scenario_name,
                                       scenario_names_list,
                                       out_path,
                                       PARALLEL,
                                       yrs,
                                       subgrp_levels) {

  raw_data <-
    read_valid_runs_scenario(scenario_name,
      scenario_names_list = scenario_names_list,
      out_path = out_path,
      PARALLEL = PARALLEL
    )

  ever_stats <- lapply(yrs,
         function(x) {
           if (as.numeric(x)==0) {
             outdf <- calculate_main_manu2_table(
               filter(raw_data$statsDFList$deadStats, year==0),
               filter(raw_data$statsDFList$liveStats, year==0))
           } else {
             outdf <- calculate_main_manu2_table(
               filter(raw_data$statsDFList$deadStats, year!=0, year<=as.numeric(x)),
               filter(raw_data$statsDFList$liveStats, year!=0, year<=as.numeric(x)))
           }

           outdf <- filter(outdf, year=="Ever") %>%
             mutate(year = as.numeric(x))

           return(outdf)
         }
  ) %>%
    bind_rows() %>%
    select(year, pct_suppress, pct_retain, pct_ever_care) %>%
    pivot_longer(c(pct_suppress, pct_retain, pct_ever_care), names_to = "stat") %>%
    mutate(year = paste("year", year, sep = "_")) %>%
    pivot_wider(names_from = year)

  calculated_scenario <-
    gather_scenario_equity_table_data(
      years = yrs,
      subgrp_levels = subgrp_levels,
      simData = raw_data$simData,
      statsDFList = raw_data$statsDFList
    )

  calculated_scenario$none <- bind_rows(calculated_scenario$none, ever_stats)

rw_clients_df <- filter(raw_data$statsDFList$liveStats, year %in% yrs) %>%
  group_by(year) %>%
  mutate(tot_pospop = n()) %>%
  filter(rwhap == 1) %>%
  mutate(
    rw_pospop = n(),
    rw_pct = rw_pospop / tot_pospop
  )

rw_clients_calcs <- lapply(
  subgrp_levels,
  function(sg) {
    if (sg == "none") {
      out_df <- distinct(rw_clients_df, year, tot_pospop, rw_pospop, rw_pct) %>%
        pivot_longer(-year)
    } else {
      out_df <- group_by(rw_clients_df, year, !!sym(sg)) %>%
        summarize(
          pospop = n(),
          rw_pospop = first(rw_pospop),
          tot_pospop = first(tot_pospop),
          .groups = "drop"
        ) %>%
        mutate(
          rw_sub_pct = pospop / rw_pospop * 100,
          tot_sub_pct = pospop / tot_pospop * 100
        ) %>%
        pivot_longer(c(-year, -!!sg))
    }
    mutate(out_df, year = paste0("year_", year)) %>%
      pivot_wider(names_from = year) %>%
      rename(stat = name) %>%
      select(stat, everything()) %>%
      return()
  }
) %>%
  `names<-`(subgrp_levels)

  stopifnot(names(calculated_scenario)==names(rw_clients_calcs))

  out_list <- map2(calculated_scenario, rw_clients_calcs, bind_rows)

  return(out_list)
}

extract_outcomes_to_bootstrap_by_subgrp <- function(scenario) {
  lapply(names(scenario),
    function(subgrp, scenario) {
      if (subgrp == "none") {
        outcomes <- filter(scenario[[subgrp]], stat %in% c(
          "newinfects_disc", "newdeaths_disc", "qaly_disc", "totalcost_disc",
          "ever_6", "ever_4_or_5", "ever_5", "ever_4_or_5_pct", "ever_5_pct",
          "pospopsize", "care", "suppress",
          "left", "pct_suppress", "pct_retain", "pct_ever_care", "snap_care",
          "snap_care_pct", "snap_suppress", "snap_suppress_pct",
          "snap_care_suppress", "snap_care_suppress_pct", "rw_pospop", "rw_pct"
        ))
        cost_per_qaly <- filter(
          outcomes,
          stat %in% c("totalcost_disc", "qaly_disc")
        ) %>%
          arrange(stat) %>%
          select(-stat) %>%
          apply(2, function(x) x[2] / x[1]) %>%
          t() %>%
          as_tibble() %>%
          mutate(stat = "cost_per_qaly")
        outcomes <- bind_rows(outcomes, cost_per_qaly) %>%
          mutate(category = "all")
      } else {
        outcomes <- filter(scenario[[subgrp]], stat %in% c(
          "newinfects", "ever_4_or_5_ti",
          "ever_5_ti", "qaly_ti",
          "pospopsize", "ever_4_or_5_sg_pct",
          "ever_5_sg_pct", "snap_care", "snap_care_sg_pct",
          "snap_suppress", "snap_suppress_sg_pct",
          "snap_care_suppress", "snap_care_suppress_sg_pct",
          "qaly_disc", "rw_sub_pct", "pospop"
        )) %>%
          mutate(category = !!sym(subgrp)) %>%
          select(-!!subgrp)
      }
      return(outcomes)
    },
    scenario = scenario
  ) %>%
    `names<-`(names(scenario)) %>%
    bind_rows(.id = "subgrp")
}

calculate_scenario_outcome_diffs <- function(rw_nm, nrw_nm, scenario_outcomes, yrs) {
  rw <- scenario_outcomes[[rw_nm]]
  nrw <- scenario_outcomes[[nrw_nm]]

  yr_vars <- paste("year", yrs, sep = "_")
  diffs <- left_join(rw, nrw, by = c("subgrp", "stat", "category"), suffix = c("_rw", "_nrw"))
  diffs <- right_join(diffs,
    lapply(yr_vars,
      function(yr, diffs) {
        diff_df <- mutate(diffs,
                          !!sym(paste0(yr, "_diff")) :=
                            !!sym(paste0(yr, "_rw")) - !!sym(paste0(yr, "_nrw"))) %>%
          select(subgrp, stat, category, !!paste0(yr, "_diff"))
        icer <- filter(diff_df, stat %in% c("totalcost_disc", "qaly_disc"),
                       category == "all") %>%
          arrange(stat) %>%
          select(-stat, -subgrp, -category) %>%
          apply(2, function(x) x[2] / x[1]) %>%
          t() %>%
          as_tibble() %>%
          mutate(subgrp = "none", stat = "icer", category = "all")
        return(bind_rows(diff_df, icer) %>%
                 arrange(subgrp, stat, category))
      },
      diffs = diffs
    ) %>% reduce(inner_join, by = c("subgrp", "stat", "category")),
    by = c("subgrp", "stat", "category")
  ) %>%
    select(subgrp, stat, category, everything()) %>%
    arrange(subgrp, stat, category)
  for (yr in yr_vars) {
    diffs <- mutate(diffs, !!sym(paste0(yr, "_rw")) :=
                      if_else(stat == "icer",
                              !!sym(paste0(yr, "_diff")),
                              !!sym(paste0(yr, "_diff"))/!!sym(paste0(yr, "_nrw")))
    )
  }
  diffs <- mutate(diffs, stat = if_else(stat=="icer", stat, paste0(stat, "_diff")))
  return(diffs)
}

reshape_outcomes_wide <- function(outcomes_df) {
  pivot_longer(outcomes_df, contains("year")) %>%
    unite(final_name, subgrp, category, stat, name) %>%
    pivot_wider(names_from = final_name)
}

reshape_diffs_wide_and_cleanup <- function(diffs) {
  select(diffs, -contains("_diff"), -contains("_nrw")) %>%
    filter(stat %in% c(
      "icer",
      "ever_5_sg_pct_diff",
      "ever_5_pct_diff",
      "ever_4_or_5_sg_pct_diff",
      "ever_4_or_5_pct_diff"
    )) %>%
    reshape_outcomes_wide() %>%
    select(-matches("icer_year_[0-9]{1,2}_nrw"))
}

calc_summary_stats_across_runs <- function(scenario_names, scenario_outcomes) {
  lapply(scenario_outcomes[scenario_names],
         reshape_outcomes_wide) %>%
    bind_rows() %>%
    map(custom_summary) %>%
    bind_rows(.id = "stat")
}

custom_summary <- function(x) {
  q5 <- sort(x)[max(1, round(0.05 * length(sort(x))))]
  q2.5 <- sort(x)[max(1, round(0.025 * length(sort(x))))]
  q95 <- sort(x)[max(1, min(length(sort(x)), round(0.95 * length(sort(x)))))]
  q97.5 <- sort(x)[max(1, min(length(sort(x)), round(0.975 * length(sort(x)))))]
  c(
    min = min(x, na.rm = TRUE),
    q2.5 = if_else(is.na(q2.5), NA_real_, as.numeric(q2.5)),
    q5 = if_else(is.na(q5), NA_real_, as.numeric(q5)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    q95 = if_else(is.na(q95), NA_real_, as.numeric(q95)),
    q97.5 = if_else(is.na(q97.5), NA_real_, as.numeric(q97.5)),
    max = max(x, na.rm = TRUE),
    cntNA = sum(is.na(x)),
    n = length(x)
  )
}

build_table_2_total_row <- function(data, stat, yrs = c(0, 10), pull_val = "median") {
  rw <- build_table_2_rwhap_component(data, F, stat, 1, yrs, pull_val)
  nrw <- build_table_2_rwhap_component(data, F, stat, 0, yrs, pull_val)
  left_join(rw, nrw, by = c("Stat", "Category"), suffix = c("_rw", "_nrw"))
}

build_table_2_yr_column <- function(data, subgrps, stat, ispct, year, rwhap, pull_val = "median") {
  if (subgrps) {
    out_df <- filter(data, subgrp != "none", stat == !!stat, year == !!year, rwhap == !!rwhap) %>%
      select(
        Stat = subgrp, Category = category,
        !!sym(paste("Year", year, sep = "_")) := !!sym(pull_val)
      )
  } else {
    out_df <- filter(data, subgrp == "none", stat == !!stat, year == !!year, rwhap == !!rwhap) %>%
      mutate(
        Stat = "Total",
        Category = "All"
      ) %>%
      select(Stat, Category, !!sym(paste("Year", year, sep = "_")) := !!sym(pull_val))
  }

  if (ispct) {
    out_df <- mutate(
      out_df,
      !!sym(paste("Year", year, sep = "_")) :=
        !!sym(paste("Year", year, sep = "_")) * 100
    )
  }

  return(out_df)
}

build_table_2_rwhap_component <- function(data, subgrps, stat, rwhap, yrs = c(0, 10), pull_val = "median") {
  if (grepl("_pct", stat)) {
    ispct <- T
  } else {
    ispct <- F
  }
  c0 <- build_table_2_yr_column(data, subgrps, stat, ispct, yrs[1], rwhap, pull_val)
  c10 <- build_table_2_yr_column(data, subgrps, stat, ispct, yrs[2], rwhap, pull_val)
  left_join(c0, c10, by = c("Stat", "Category")) %>%
    mutate(
      Percent =
        (!!sym(paste0("Year_", yrs[2])) -
          !!sym(paste0("Year_", yrs[1]))) / !!sym(paste0("Year_", yrs[1])) * 100
    )
}

build_table_2 <- function(data, stat, yrs = c(0, 10), pull_val = "median") {
  if (stat == "qaly_disc") {
    tr <- build_table_2_total_row(data, "qaly_disc", yrs, pull_val)
  } else {
    tr_row_stat <- gsub("_sg_", "_", stat)
    if (!(tr_row_stat %in% data$stat)) {
      stop(paste(tr_row_stat, "data is missing, or total row data for", stat,
                 "is stored in a different variable or has not been created yet."))
    } else {
      tr <- build_table_2_total_row(data, tr_row_stat, yrs, pull_val)
    }
  }

  rw <- build_table_2_rwhap_component(data, T, stat, 1, yrs, pull_val)
  nrw <- build_table_2_rwhap_component(data, T, stat, 0, yrs, pull_val)
  bind_rows(
    tr,
    left_join(rw, nrw,
      by = c("Stat", "Category"),
      suffix = c("_rw", "_nrw")
    )
  ) %>%
    order_table_rows()
}

order_table_rows <- function(tbl_data, grpnm = "Stat", catnm = "Category") {
  mutate(tbl_data,
    !!sym(grpnm) := factor(!!sym(grpnm),
      levels = c("Total", "agegroup", "gender", "risk", "race")
    ),
    !!sym(catnm) := factor(!!sym(catnm),
      levels = c(
        "All", "youth", "adult", "olderadult",
        "male", "female",
        "MSM", "IDU", "MSMandIDU", "black", "hispanic",
        "other"
      )
    )
  ) %>%
    arrange(!!sym(grpnm), !!sym(catnm))
}



