library(gtools)
library(ensurer)
library(truncnorm)
library(assertthat)
library(rlang)
library(readxl)
library(tidyverse)
library(tictoc)
library(reticulate)
library(fastRG)
library(mice)

for (fl in list.files("./modules")) {
  print(fl)
  file_type = str_split(fl, "\\.")[[1]][2]

  if (file_type == "R") {
    source(file.path(".", "modules", fl))
  }
  if (file_type == "py") {
    reticulate::source_python(file.path(".", "modules", fl))
  }
}

for (excel_file in (list.files("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Inputs_files_03_09_24_no_int"))[4]) {

#file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/RWHAP_Equity-feat-add_equity_outcomes/inputs_2019/user_inputs_Current_RWHAP - 200K - PrEP.xlsx"
#file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/SD_data/data.best.epi_11_10_23.xlsx"
file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/SD_data/data_10_15_2024.xlsx"
#file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/SD_data/data_error_v1.xlsx"
#file_loc_input = paste("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Inputs_files_03_09_24_no_int/", excel_file, sep = "")

file_loc_link = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/SD_data/sd_demographics_20240821.csv"
#file_loc_link = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/SD_data/sd_county_demographics.csv"

file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/SD_data/sd_data_20250306_O10Y2.xlsx"
file_loc_input = "/Users/ravigoyal/Downloads/001/inputs/data.xlsx"

file_loc_link = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/miami_demographics_20241125.csv"
file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration/miami_data_20241208_v11_intervention.xlsx"
#file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration/miami_data_20241208_v11.xlsx"

file_loc_link = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration_4_1_2025/Miami_demographics_20250325.csv"
#file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration_4_1_2025/miami_data_20241202_O1Y1_epigen_TT.xlsx"
file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Results/Incorrect_age/data_MSMincrease.xlsx"
file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Results/Incorrect_age/data_scenario.xlsx"
file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/RWHAP_analysis/MIA_epigen_O7Y7_calibrated_input_15yrs.xlsx"


diag_time_demo_sum_all.df = NULL
trans_tree_demo_sum_all.df = NULL
simData_sum_all.df = NULL

Miamiflag   <- read_cell(file_loc_input, "High Level Pop + Sim Features", "E30")
migrationflag   <- read_cell(file_loc_input, "High Level Pop + Sim Features", "E31")

for (sim_iter in c(1:50)) {

inputObj <- input_module(origin = file_loc_input)

set.seed(inputObj$seed)

inputObj$testflag <- TRUE
inputObj$valflag  <- FALSE

#set.seed(inputObj$seed)

tic()
simObj   <- initialization_module(inputObj)
toc()

###################

#pre_neutral
simObj$HIVtesting$pre_neutral$num_tests   <- read_cell(file_loc_input, "Testing",   "B4")
simObj$HIVtesting$pre_neutral$num_tests_neutral  <- read_cell(file_loc_input, "Testing",   "B5")
simObj$HIVtesting$pre_neutral$prep_referal_uptake_prop    <- read_cell(file_loc_input, "Testing",   "B6")
simObj$HIVtesting$pre_neutral$start    <- read_cell(file_loc_input, "Testing",   "B8")

#scale_neutral
simObj$HIVtesting$scale_neutral$num_tests   <- read_cell(file_loc_input, "Testing",   "C4")
simObj$HIVtesting$scale_neutral$num_tests_neutral  <- read_cell(file_loc_input, "Testing",   "C5")
simObj$HIVtesting$scale_neutral$prep_referal_uptake_prop    <- read_cell(file_loc_input, "Testing",   "C6")
simObj$HIVtesting$scale_neutral$start    <- read_cell(file_loc_input, "Testing",   "C8")

#maintain_neutral
simObj$HIVtesting$maintain_neutral$num_tests   <- read_cell(file_loc_input, "Testing",   "D4")
simObj$HIVtesting$maintain_neutral$num_tests_neutral  <- read_cell(file_loc_input, "Testing",   "D5")
simObj$HIVtesting$maintain_neutral$prep_referal_uptake_prop    <- read_cell(file_loc_input, "Testing",   "D6")
simObj$HIVtesting$maintain_neutral$test_category    <- read_cell(file_loc_input, "Testing",   "D7")
simObj$HIVtesting$maintain_neutral$start    <- read_cell(file_loc_input, "Testing",   "D8")
simObj$HIVtesting$maintain_neutral$pos_neg_test_ratio    <- read_cell(file_loc_input, "Testing",   "D9")

if (simObj$HIVtesting$maintain_neutral$test_category == "geo") {
  simObj$HIVtesting$pre_neutral$num_tests_cat <- read_excel(file_loc_input, "Testing", "B11:F24")
  simObj$HIVtesting$scale_neutral$num_tests_cat <- bind_cols(simObj$HIVtesting$pre_neutral$num_tests_cat %>% select("group"), read_excel(file_loc_input, "Testing", "G11:J24"))
  simObj$HIVtesting$maintain_neutral$num_tests_cat <- bind_cols(simObj$HIVtesting$pre_neutral$num_tests_cat %>% select("group"), read_excel(file_loc_input, "Testing", "K11:N24"))
} else if (simObj$HIVtesting$maintain_neutral$test_category == "risk") {
  simObj$HIVtesting$pre_neutral$num_tests_cat <- read_excel(file_loc_input, "Testing", "B26:F30")
  simObj$HIVtesting$scale_neutral$num_tests_cat <- bind_cols(simObj$HIVtesting$pre_neutral$num_tests_cat %>% select("group"), read_excel(file_loc_input, "Testing", "G26:J30"))
  simObj$HIVtesting$maintain_neutral$num_tests_cat <- bind_cols(simObj$HIVtesting$pre_neutral$num_tests_cat %>% select("group"), read_excel(file_loc_input, "Testing", "K26:N30"))
} else if (simObj$HIVtesting$maintain_neutral$test_category == "race") {
  simObj$HIVtesting$pre_neutral$num_tests_cat <- read_excel(file_loc_input, "Testing", "B32:F35")
  simObj$HIVtesting$scale_neutral$num_tests_cat <- bind_cols(simObj$HIVtesting$pre_neutral$num_tests_cat %>% select("group"), read_excel(file_loc_input, "Testing", "G32:J35"))
  simObj$HIVtesting$maintain_neutral$num_tests_cat <- bind_cols(simObj$HIVtesting$pre_neutral$num_tests_cat %>% select("group"), read_excel(file_loc_input, "Testing", "K32:N35"))
} else {
  print("Error!!")
}

simObj$stagetransprobs$hiv = tibble(
  group = simObj$HIVtesting$pre_neutral$num_tests_cat$group,
  stage = "hiv",
  betters = 0,
  bettere = 0,
  btime = 0,
  worses = 0,
  worsee = 0,
  wtime = 0
)
colnames(simObj$stagetransprobs$hiv)[1] = simObj$HIVtesting$maintain_neutral$test_category
colnames(simObj$HIVtesting$pre_neutral$num_tests_cat)[1] = simObj$HIVtesting$maintain_neutral$test_category
colnames(simObj$HIVtesting$scale_neutral$num_tests_cat)[1] = simObj$HIVtesting$maintain_neutral$test_category
colnames(simObj$HIVtesting$maintain_neutral$num_tests_cat)[1] = simObj$HIVtesting$maintain_neutral$test_category

###################

simObj   <- initialize_prep(simObj,
                            origin = file_loc_input)

simObj$notrans_tree.df <- tibble(ID1 = "None",
                        ID2 = simObj$popdf$id,
                        month = 0)

simObj$trans_tree <- tibble(ID1 = integer(),
                            ID2 = integer(),
                            month = integer())

simObj$diag_time <- tibble(ID = simObj$popdf %>%
                             filter(stage %in% c("suppress", "left", "diag", "care", "dead")) %>%
                             pull(id),
                           month = 0,
                           event = "initial",
                           cd4 = simObj$popdf %>%
                             filter(stage %in% c("suppress", "left", "diag", "care", "dead")) %>%
                             pull(cd4))

simObj$popdf_dead = NULL
simObj$popdf_migrate = NULL

if (!is.null(file_loc_link)) {
  link_county_abm.df = link_create(file_loc_link, simObj)

  #sprintf("Linkages...")
  #link_county_abm.df %>% as.data.frame() %>% print(quote = FALSE, row.names = FALSE)

}

link_county_abm.df %>% is.na() %>% sum()

# simObj$popdf %>% nrow()
#
# simObj$popdf %>%
#   group_by(stage) %>%
#   summarise(n_carestage = n())
#
# simObj$popdf %>%
#   filter(stage == "suppress") %>%
#   group_by(agegroup, gender, race, risk) %>%
#   summarise(n_demo = n()) %>%
#   ungroup() %>%
#   mutate(n_stage = sum(n_demo)) %>%
#   mutate(p_demo = n_demo / n_stage) %>%
#   View()

# num_tests.df = bind_rows(simObj$popdf, simObj$negpopdf) %>%
#   group_by(risk, gender) %>%
#   summarize(risk_pop = n()) %>%
#   left_join(simObj$stagetransprobs$hiv, by = c("gender", "risk")) %>%
#   select(c(gender, risk, risk_pop, betters)) %>%
#   mutate(num_tests = risk_pop*betters)

####

simData <- data.frame(list())

h_MSM = c()
source("~/Dropbox/Academic/Research/Projects/ABM_HIV/abm_hiv/testing_networks.R")

print("Starting simulation...")
print(paste("Month: ", "0", sep = ""))
if (simObj$duration < 1) {
  simObj <- outcomes_module(simObj)
  simData <- bind_rows(simData, collapse_module(simObj))
} else {
  for (i in 1:simObj$duration) {
    tic()
    print(paste("Month: ", i, sep = ""))

    simObj <- increment_module(simObj)

    test_rate_change_bool = 0
    if (simObj$month == simObj$HIVtesting$pre_neutral$start) {
      simObj$HIVtesting$num_tests   <- simObj$HIVtesting$pre_neutral$num_tests
      simObj$HIVtesting$num_tests_neutral  <- simObj$HIVtesting$pre_neutral$num_tests_neutral
      simObj$HIVtesting$prep_referal_uptake_prop    <- simObj$HIVtesting$pre_neutral$prep_referal_uptake_prop
      simObj$HIVtesting$num_tests_cat <- simObj$HIVtesting$pre_neutral$num_tests_cat
      simObj$HIVtesting$test_category = simObj$HIVtesting$maintain_neutral$test_category
      simObj$HIVtesting$pos_neg_test_ratio = simObj$HIVtesting$maintain_neutral$pos_neg_test_ratio
      test_rate_change_bool = 1
    } else if (simObj$month == simObj$HIVtesting$scale_neutral$start) {
      simObj$HIVtesting$num_tests   <- simObj$HIVtesting$scale_neutral$num_tests
      simObj$HIVtesting$num_tests_neutral  <- simObj$HIVtesting$scale_neutral$num_tests_neutral
      simObj$HIVtesting$prep_referal_uptake_prop    <- simObj$HIVtesting$scale_neutral$prep_referal_uptake_prop
      simObj$HIVtesting$num_tests_cat <- simObj$HIVtesting$scale_neutral$num_tests_cat
      simObj$HIVtesting$test_category = simObj$HIVtesting$maintain_neutral$test_category
      simObj$HIVtesting$pos_neg_test_ratio = simObj$HIVtesting$maintain_neutral$pos_neg_test_ratio
      test_rate_change_bool = 1
    } else if (simObj$month == simObj$HIVtesting$maintain_neutral$start) {
      simObj$HIVtesting$num_tests   <- simObj$HIVtesting$maintain_neutral$num_tests
      simObj$HIVtesting$num_tests_neutral  <- simObj$HIVtesting$maintaine_neutral$num_tests_neutral
      simObj$HIVtesting$prep_referal_uptake_prop    <- simObj$HIVtesting$maintain_neutral$prep_referal_uptake_prop
      simObj$HIVtesting$num_tests_cat <- simObj$HIVtesting$scale_neutral$num_tests_cat
      simObj$HIVtesting$test_category = simObj$HIVtesting$maintain_neutral$test_category
      simObj$HIVtesting$pos_neg_test_ratio = simObj$HIVtesting$maintain_neutral$pos_neg_test_ratio
      test_rate_change_bool = 1
    }

    if (test_rate_change_bool == 1) {
      simObj$stagetransprobs$hiv

      group_testing.df = simObj$HIVtesting$num_tests_cat %>% left_join(simObj$popdf %>%
                                                                         filter(stage == "hiv") %>%
                                                                         group_by(across(all_of(simObj$HIVtesting$test_category))) %>%
                                                                         summarize(group_pop = n())) %>%
        mutate(test_prob = total_num*simObj$HIVtesting$pos_neg_test_ratio/group_pop)

      simObj$stagetransprobs$hiv$betters = group_testing.df$test_prob
      simObj$stagetransprobs$hiv$bettere = group_testing.df$test_prob
    }

    # if (simObj$month == 121) {
    #   #Change in RWHAP
    #
    #   #calculate probability
    #   rwhap_removal_prob = 3600/(simObj$popdf %>%
    #     filter(stage %in% c("care", "suppress")) %>%
    #     nrow() %>% sum())
    #
    #   #change in current population probability
    #   simObj$popdf$rhwap_change = rbinom(n = nrow(simObj$popdf), size = 1, prob = rwhap_removal_prob)
    #   for (hiv_id in c(1:nrow(simObj$popdf))) {
    #     if (simObj$popdf$rhwap_change[hiv_id] == 1) {
    #       simObj$popdf$benemcm[hiv_id] = 0
    #       simObj$popdf$benemhsa[hiv_id] = 0
    #       simObj$popdf$benesupport[hiv_id] = 0
    #       simObj$popdf$oahsart[hiv_id] = 0
    #       if (simObj$popdf$stage %in% c("care", "suppress")) {
    #         simObj$popdf$stage[hiv_id] = "left"
    #       }
    #     }
    #   }
    #
    #   #change in current population stage
    #
    #   #change in future population probability
    #
    # }

    simObj <- transmission_module(simObj)
    simObj <- care_stage_module(simObj)
    simObj <- health_state_module(simObj)
    simObj <- outcomes_module(simObj)
    simObj <- prep_update(simObj)
    if (migrationflag == TRUE) {
      simObj <- migration(simObj)
    }
    simData <- bind_rows(simData, collapse_module(simObj))
    h_MSM = c(h_MSM, homophily_check(simObj))
    toc()
  }
}


######

simData <- inflate_module(simData, simObj$inflation)
simDataDisc <- discount_module(simData, simObj$discount)

simData <- list(notdisc = simData,
                disc    = simDataDisc)

saveRDS(simData, "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Presentation/Status_quo.RDS")

sprintf("Printing output...")

sprintf("Calibration metrics...")

calibration_output = left_join(
  simObj$diag_time %>% filter(event == "diagnosis"),
  bind_rows(simObj$popdf %>% select(id, gender, risk, age, race, geo),
            simObj$popdf_dead),
  by = join_by(ID == id)) %>%
  group_by(risk, month) %>%
  summarise(newinfects_agg = n()) %>%
  mutate(metric = "newinfects_agg") %>%
  rename("subgroup" = "risk",
         "stat" = "newinfects_agg") %>%
  select(metric, month, subgroup, stat)

calibration_output = calibration_output  %>%
  mutate(year = trunc((month-1)/12)) %>%
  group_by(subgroup, year) %>%
  summarize(total_year_sim = sum(stat))

#calibration_output_ind = calibration_output

calibration_output = left_join(
  simObj$diag_time %>% filter(event == "diagnosis"),
  bind_rows(simObj$popdf %>% select(id, gender, risk, age, race, geo),
            simObj$popdf_dead),
  by = join_by(ID == id)) %>%
  group_by(race, month) %>%
  summarise(newinfects_agg = n()) %>%
  mutate(metric = "newinfects_agg") %>%
  rename("subgroup" = "race",
         "stat" = "newinfects_agg") %>%
  select(metric, month, subgroup, stat)


##########
##########

infect_output <- simData$notdisc %>%
  group_by(risk, month) %>%
  summarise(newinfects_agg = sum(newinfects)) %>%
  pivot_longer(cols = c(newinfects_agg),
               names_to = "metric",
               values_to = "stat") %>%
  ungroup() %>%
  rename(subgroup = risk) %>%
  select(metric, month, subgroup, stat)

infect_output_risk = infect_output  %>%
  mutate(year = trunc((month-1)/12)) %>%
  group_by(subgroup, year) %>%
  summarize(total_year_sim = sum(stat))

calibration_output_equal_group = infect_output
write_csv(calibration_output_equal_group, "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration/miami_calibration_v11_equal_group.csv")

#calibration_output_equal_ind = read_csv("/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration/miami_calibration_v11_equal_ind.csv")
#calibration_output_equal_group = read_csv("/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration/miami_calibration_v11_equal_group.csv")

infect_output$total_year_equal_group = calibration_output_equal_group$total_year_sim
infect_output$total_year_equal_ind = calibration_output_equal_ind$total_year_sim

infect_output_risk$year = infect_output_risk$year + 2016
infect_output_risk %>% View()

infect_output_risk_wide = infect_output_risk %>% pivot_wider(
  names_from = year,
  values_from = total_year_sim
)

infect_output <- simData$notdisc %>%
  group_by(race, month) %>%
  summarise(newinfects_agg = sum(newinfects)) %>%
  pivot_longer(cols = c(newinfects_agg),
               names_to = "metric",
               values_to = "stat") %>%
  ungroup() %>%
  rename(subgroup = race) %>%
  select(metric, month, subgroup, stat)

infect_output_race = infect_output  %>%
  mutate(year = trunc((month-1)/12)) %>%
  group_by(subgroup, year) %>%
  summarize(total_year_sim = sum(stat))

infect_output_race$year = infect_output_race$year + 2016
infect_output_race %>% View()

infect_output_race_wide = infect_output_race %>% pivot_wider(
  names_from = year,
  values_from = total_year_sim
)

write_csv(bind_rows(infect_output_race_wide, infect_output_risk_wide),
          file = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/bruce_table.csv")


infect_output_TEMP = infect_output %>%
  mutate(total_year_sim = ifelse(year >= 2025, NA, total_year_sim))

custom_labels <- c(
  "MSM" = "MSM",
  "MSMandIDU" = "MSM and PWID",
  "IDU" = "PWID",
  "other" = "Heterosexual"
)

ggplot(infect_output_TEMP, aes(x = year)) +
  geom_line(aes(y = total_year_equal_ind, color = "Status Quo"), size = 1) +
  geom_line(aes(y = total_year_equal_group, color = "Intervention"), size = 1) +
  geom_line(aes(y = total_year_sim, color = "Pre-intervention"), size = 1) +
  geom_vline(xintercept = 2025, linetype = "dotted", color = "black", size = 1) +
  scale_y_continuous(limits = c(0, 900)) +
  facet_wrap(~ subgroup, scales = "free_y", labeller = labeller(subgroup = custom_labels)) +
  labs(
    title = "Intervention Comparison by Subgroup",
    x = "Year",
    y = "Total",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

##########
##########

file_loc_input = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration/miami_calibration_20241202.csv"
calibration_miami = read_csv(file_loc_input) %>%
  separate(description, into = c("year", "subgroup"), sep = "_") %>%
  filter(weight == 1) %>%
  mutate(year = as.numeric(year) - 2016) %>%
  rename(total_year_obs = value) %>%
  select(-weight) %>%
  mutate(subgroup = ifelse(subgroup == "Other", "other", subgroup))

calibration_compare = left_join(calibration_output,
                                calibration_miami,
                                by = c("year", "subgroup")) %>%
  mutate(diff = total_year_sim - total_year_obs)

calibration_compare %>% View()
score = sum((calibration_compare$diff)^2)
score

write_csv(calibration_compare, "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/Calibration/miami_calibration_v11.csv")

custom_labels <- c(
  "MSM" = "MSM",
  "MSMandIDU" = "MSM and PWID",
  "IDU" = "PWID",
  "other" = "Heterosexual"
)

ggplot(calibration_compare, aes(x = year+2016)) +
  geom_line(aes(y = total_year_sim, color = "Simulated"), size = 1) +
  geom_point(aes(y = total_year_obs, color = "Observed"), shape = 3, size = 3) +
  scale_y_continuous(limits = c(0, 850)) +
  facet_wrap(~ subgroup, scales = "free_y", labeller = labeller(subgroup = custom_labels)) +
  labs(
    title = "Calibration Comparison by Subgroup",
    x = "Year",
    y = "Total",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

######
print(paste("#########sim_iter: ", sim_iter, sep = ""))

diag_time_demo.df = left_join(
  simObj$diag_time %>% filter(event == "diagnosis"),
  bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
            simObj$popdf_dead),
                               by = join_by(ID == id))

diag_time_demo.df = diag_time_demo.df %>%
  mutate(year = ceiling(month/12)+2018)

diag_time_demo_sum.df = diag_time_demo.df %>%
  group_by(month, risk, year) %>%
  summarize(diag = n()) %>%
  ungroup() %>%
  group_by(risk, year) %>%
  summarize(tot_diag = sum(diag)) %>%
  mutate(sim_iter = sim_iter)

diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all.df,
                                      diag_time_demo_sum.df)

diag_time_demo_sum_all.df = diag_time_demo_sum_all.df %>%
  arrange(year)

##

trans_tree_demo.df = left_join(simObj$trans_tree, bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
                                                            simObj$popdf_dead) %>% select(id, risk),
                               by = join_by(ID2 == id))

trans_tree_demo.df = trans_tree_demo.df %>%
  mutate(year = ceiling(month/12)+2018)

trans_tree_demo_sum.df = trans_tree_demo.df %>%
  group_by(month, risk, year) %>%
  summarize(infects = n()) %>%
  ungroup() %>%
  group_by(risk, year) %>%
  summarize(tot_infects = sum(infects)) %>%
  mutate(sim_iter = sim_iter)

trans_tree_demo_sum_all.df = bind_rows(trans_tree_demo_sum_all.df,
                                       trans_tree_demo_sum.df)


##

simData_sum.df = simData %>%
  group_by(month) %>%
  filter(stage %in% c("care", "diag", "left", "suppress")) %>%
  summarise(total_diag = sum(pospopsize),
            supp_diag = sum(pospopsize[stage == "suppress"])) %>%
  mutate(supp_prop = supp_diag / total_diag) %>%
  mutate(year = ceiling(month/12)+2018) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(mean_supp_prop_annual = mean(supp_prop))

simData_sum_all.df = bind_rows(simData_sum_all.df,
                               simData_sum.df)
}

# saveRDS(diag_time_demo_sum_all.df,
#         paste("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/", excel_file, "_iter20_diag_no_int.rds", sep = "")
#         )
# saveRDS(trans_tree_demo_sum_all.df,
#         paste("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/", excel_file, "_iter20_infects_no_int.rds", sep = "")
#         )
# saveRDS(simData_sum.df,
#         paste("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/", excel_file, "_iter20_suppress_prop_no_int.rds", sep = "")
#         )

}

#####

library(svglite)

# diag_time_demo_sum_all_eg.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_CFAR_presentation/epi_genetic_11_13_23_iter20_diag.rds") %>%
#   mutate(sim_type = 'epi_genetic')
#
# diag_time_demo_sum_all_e.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_CFAR_presentation/epi_11_13_23_iter20_diag.rds") %>%
#   mutate(sim_type = 'epi')
#
# diag_time_demo_sum_all_eg_i.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_CFAR_presentation/epi_genetic_11_13_23_iter20_mod_2024_diag.rds") %>%
#   mutate(sim_type = 'epi_genetic_int')
#
# diag_time_demo_sum_all_e_i.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_CFAR_presentation/epi_11_13_23_iter20_mod_2024_diag.rds") %>%
#   mutate(sim_type = 'epi_int')


input_file_counter = 1
diag_time_demo_sum_all_e_i.df = NULL

for (excel_file in list.files("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Inputs_files_03_09_24")) {

  if (excel_file == "data_O3Y3_epigen.xlsx") {
    diag_time_demo_sum_all_eg_i.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/data_O3Y3_epigen.xlsx_iter20_diag_int.rds") %>%
      mutate(sim_type = 'epi_genetic_int') %>%
      mutate(input_file = input_file_counter)
  } else {
    #if (excel_file == "data_O6Y7.xlsx") {

    file_loc_input = paste("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/", excel_file, sep = "")

    diag_time_demo_sum_all_e_i_temp.df = readRDS(paste(file_loc_input, "_iter20_diag_int.rds", sep = "")) %>%
      mutate(sim_type = 'epi_int') %>%
      mutate(input_file = input_file_counter)

    diag_time_demo_sum_all_e_i.df = bind_rows(diag_time_demo_sum_all_e_i.df,
                                                   diag_time_demo_sum_all_e_i_temp.df)
    #}
  }
  input_file_counter = input_file_counter + 1
}

input_file_counter = 1
diag_time_demo_sum_all_e.df = NULL

for (excel_file in list.files("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Inputs_files_03_09_24_no_int")) {

  if (excel_file == "data_O3Y3_epigen.xlsx") {
    diag_time_demo_sum_all_eg.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/data_O3Y3_epigen.xlsx_iter20_diag_no_int.rds") %>%
      mutate(sim_type = 'epi_genetic') %>%
      mutate(input_file = input_file_counter)
  } else {
    #if (excel_file == "data_O6Y7.xlsx") {

    file_loc_input = paste("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/", excel_file, sep = "")

    diag_time_demo_sum_all_e_temp.df = readRDS(paste(file_loc_input, "_iter20_diag_no_int.rds", sep = "")) %>%
      mutate(sim_type = 'epi') %>%
      mutate(input_file = input_file_counter)

    diag_time_demo_sum_all_e.df = bind_rows(diag_time_demo_sum_all_e.df,
                                              diag_time_demo_sum_all_e_temp.df)
    #}
  }
  input_file_counter = input_file_counter + 1
}


diag_time_demo_sum_all_e.df = bind_rows(diag_time_demo_sum_all_e.df, diag_time_demo_sum_all_e_i.df)
diag_time_demo_sum_all_eg.df = bind_rows(diag_time_demo_sum_all_eg.df, diag_time_demo_sum_all_eg_i.df)
diag_time_demo_sum_all.df = bind_rows(diag_time_demo_sum_all_eg.df, diag_time_demo_sum_all_e.df)

SD_diag.df = read_csv("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/SD_data/sd_newDx_risk_realloc_01_08_2024.csv") %>%
  rename(year = `Diagnosis year`,
         risk = `Exposure Category`,
         mean_diag = n_adj) %>%
  mutate(sd_diag = 0) %>%
  filter(year %in% c(2019:2021)) %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSM & IDU" ~ "MSMandIDU",
                          risk == "Other" ~ "other")) %>%
  select(year, risk, mean_diag, sd_diag) %>%
  mutate(type = "Observed",
         sim_type = "Observed")

diag_time_demo_sum_mean.df = diag_time_demo_sum_all.df %>%
  group_by(risk, year, sim_type) %>%
  summarize(mean_diag = mean(tot_diag),
            sd_diag = sd(tot_diag)) %>%
  mutate(type = "Simulation")

diag_time_demo_sum_plot.df = bind_rows(diag_time_demo_sum_mean.df,
                                       SD_diag.df) %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSMandIDU" ~ "MSM & IDU",
                          risk == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

diag_time_demo_sum_plot_TEMP.df = diag_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

#####Slide 34

fig_diag_time = ggplot(diag_time_demo_sum_plot_TEMP.df, aes(x=year, y=mean_diag, colour=sim_type, group=sim_type)) +
  geom_errorbar(aes(ymin=mean_diag-1.96*sd_diag, ymax=mean_diag+1.96*sd_diag), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Number of New Diagnoses") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi", "epi_genetic"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Number of New Diagnoses Over Time\nStratified by Transmission Risk") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_diag_time

svglite(filename = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/fig_diag_time_epi_epigenetic.svg",
        width = 10, height = 5)
fig_diag_time
dev.off()

###################
###################
###################

input_file_counter = 1
inf_time_demo_sum_all_e_i.df = NULL

for (excel_file in list.files("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Inputs_files_03_09_24")) {

  if (excel_file == "data_O3Y3_epigen.xlsx") {
    inf_time_demo_sum_all_eg_i.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/data_O3Y3_epigen.xlsx_iter20_infects_int.rds") %>%
      mutate(sim_type = 'epi_genetic_int') %>%
      mutate(input_file = input_file_counter)
  } else {
    #if (excel_file == "data_O6Y7.xlsx") {
    file_loc_input = paste("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/", excel_file, sep = "")

    inf_time_demo_sum_all_e_i_temp.df = readRDS(paste(file_loc_input, "_iter20_infects_int.rds", sep = "")) %>%
      mutate(sim_type = 'epi_int') %>%
      mutate(input_file = input_file_counter)

    inf_time_demo_sum_all_e_i.df = bind_rows(inf_time_demo_sum_all_e_i.df,
                                              inf_time_demo_sum_all_e_i_temp.df)
    #}
  }
  input_file_counter = input_file_counter + 1
}

input_file_counter = 1
inf_time_demo_sum_all_e.df = NULL

for (excel_file in list.files("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Inputs_files_03_09_24_no_int")) {

  if (excel_file == "data_O3Y3_epigen.xlsx") {
    inf_time_demo_sum_all_eg.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/data_O3Y3_epigen.xlsx_iter20_infects_no_int.rds") %>%
      mutate(sim_type = 'epi_genetic') %>%
      mutate(input_file = input_file_counter)
  } else {
    #if (excel_file == "data_O6Y7.xlsx") {

    file_loc_input = paste("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/", excel_file, sep = "")

    inf_time_demo_sum_all_e_temp.df = readRDS(paste(file_loc_input, "_iter20_infects_no_int.rds", sep = "")) %>%
      mutate(sim_type = 'epi') %>%
      mutate(input_file = input_file_counter)

    inf_time_demo_sum_all_e.df = bind_rows(inf_time_demo_sum_all_e.df,
                                             inf_time_demo_sum_all_e_temp.df)
    #}
  }
  input_file_counter = input_file_counter + 1
}

# inf_time_demo_sum_all_eg.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results/epi_genetic_11_13_23_iter20_infects.rds") %>%
#   mutate(sim_type = 'epi_genetic')
#
# inf_time_demo_sum_all_e.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results/epi_11_13_23_iter20_infects.rds") %>%
#   mutate(sim_type = 'epi')
#
# inf_time_demo_sum_all_eg_i.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results/epi_genetic_11_13_23_iter20_mod_2024_infects.rds") %>%
#   mutate(sim_type = 'epi_genetic_int')
#
# inf_time_demo_sum_all_e_i.df = readRDS("/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results/epi_11_13_23_iter20_mod_2024_infects.rds") %>%
#   mutate(sim_type = 'epi_int')

inf_time_demo_sum_all_e.df = bind_rows(inf_time_demo_sum_all_e.df, inf_time_demo_sum_all_e_i.df)
inf_time_demo_sum_all_eg.df = bind_rows(inf_time_demo_sum_all_eg.df, inf_time_demo_sum_all_eg_i.df)
inf_time_demo_sum_all.df = bind_rows(inf_time_demo_sum_all_eg.df, inf_time_demo_sum_all_e.df)

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(risk, year, sim_type) %>%
  summarize(mean_inf= mean(tot_infects),
            sd_inf = sd(tot_infects)) %>%
  mutate(type = "Simulation")

inf_time_demo_sum_plot.df = inf_time_demo_sum_mean.df %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSMandIDU" ~ "MSM & IDU",
                          risk == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

#####Slide 36

fig_inf_time = ggplot(inf_time_demo_sum_plot_TEMP.df, aes(x=year, y=mean_inf, colour=sim_type, group=sim_type)) +
  geom_errorbar(aes(ymin=mean_inf-1.96*sd_inf, ymax=mean_inf+1.96*sd_inf), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi", "epi_genetic"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Number of New Infections Over Time\nStratified by Transmission Risk") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_inf_time

svglite(filename = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/fig_inf_time_epi_epigenetic.svg",
        width = 10, height = 5)
fig_inf_time
dev.off()


inf_time_demo_sum_plot_TEMP.df = inf_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_int", "epi_genetic_int"))

inf_time_demo_sum_plot_TEMP.df$sim_type <- factor(inf_time_demo_sum_plot_TEMP.df$sim_type, levels=c("Observed", "epi_int", "epi_genetic_int"))

#####Slide 37

fig_inf_time_int = ggplot(inf_time_demo_sum_plot_TEMP.df, aes(x=year, y=mean_inf, colour=sim_type, group=sim_type)) +
  geom_errorbar(aes(ymin=mean_inf-1.96*sd_inf, ymax=mean_inf+1.96*sd_inf), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Number of New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi_int", "epi_genetic_int"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Number of New Infections Over Time\nStratified by Transmission Risk: Intervention") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_inf_time_int

svglite(filename = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/fig_inf_time_epi_epigenetic_int.svg",
        width = 10, height = 5)
fig_inf_time_int
dev.off()

# inf_time_demo_sum_plot_w.df = inf_time_demo_sum_plot.df %>%
#   select(-sd_inf) %>%
#   pivot_wider(names_from = sim_type, values_from = mean_inf) %>%
#   mutate(epi_genetic_reduce = (epi_genetic-epi_genetic_int)/epi_genetic * 100,
#          epi_reduce = (epi-epi_int)/epi * 100) %>%
#   select(risk, year, epi_reduce, epi_genetic_reduce)
#
# inf_time_demo_sum_plot_l.df = inf_time_demo_sum_plot_w.df %>%
#   pivot_longer(cols = starts_with("epi"), names_to = "type", values_to = "reduce")
#
# fig_inf_time = ggplot(inf_time_demo_sum_plot_l.df, aes(x=year, y=reduce, colour=type, group=type)) +
#   #geom_errorbar(aes(ymin=mean_diag-1.96*sd_diag, ymax=mean_diag+1.96*sd_diag), colour="black", width=.1, position=pd) +
#   geom_line(position=pd) +
#   geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
#   xlab("Year") +
#   ylab("Reduction in New Infections") +
#   scale_colour_hue(name="Data",    # Legend label, use darker colors
#                    #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
#                    #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
#                    breaks=c("epi_reduce", "epi_genetic_reduce"),
#                    labels=c("Sim (E)", "Sim (E+G)"),
#                    l=40) +                    # Use darker colors, lightness=40
#   ggtitle("Number of New Infections Over Time\nStratified by Transmission Risk") +
#   expand_limits(y=0) +                        # Expand y range
#   #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
#   theme_bw() +
#   theme(legend.justification=c(1,0),
#         legend.position=c(1,0)) +
#   facet_wrap(~risk, nrow = 2, scales = "free")
#
# fig_inf_time
#
# svglite(filename = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results/fig_infect_time_epi_epigenetic.svg",
#         width = 10, height = 5)
# fig_inf_time
# dev.off()

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(risk, sim_type, year) %>%
  summarize(sum_inf= mean(tot_infects)) %>%
  filter(year %in% c(2024,2025,2026,2027,2028,2029, 2030)) %>%
  summarize(sum_inf= sum(sum_inf)) %>%
  pivot_wider(names_from = sim_type, values_from = sum_inf) %>%
  mutate(epi_genetic_reduce = (epi_genetic-epi_genetic_int)/epi_genetic * 100,
         epi_reduce = (epi-epi_int)/epi * 100) %>%
  select(risk, epi_reduce, epi_genetic_reduce)

inf_time_demo_sum_mean.df = inf_time_demo_sum_all.df %>%
  group_by(risk, sim_type, year) %>%
  summarize(sum_inf= mean(tot_infects)) %>%
  pivot_wider(names_from = sim_type, values_from = sum_inf) %>%
  mutate(epi_genetic_reduce = (epi_genetic-epi_genetic_int)/epi_genetic * 100,
         epi_reduce = (epi-epi_int)/epi * 100) %>%
  select(risk, year, epi_reduce, epi_genetic_reduce)

inf_time_demo_sum_mean_l.df = inf_time_demo_sum_mean.df %>%
  pivot_longer(cols = starts_with("epi"), names_to = "type", values_to = "reduce")

inf_time_demo_sum_mean_l.df = inf_time_demo_sum_mean_l.df %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSMandIDU" ~ "MSM & IDU",
                          risk == "other" ~ "Other")) %>%
  mutate(Simulation = case_when(type == "epi_reduce" ~ "Sim (E)",
                          type == "epi_genetic_reduce" ~ "Sim (E+G)"))

#####Slide 38

inf_time_demo_sum_mean_l.df$Simulation <- factor(inf_time_demo_sum_mean_l.df$Simulation, levels=c("Sim (E)", "Sim (E+G)"))


fig_inf_reduce_int = ggplot(inf_time_demo_sum_mean_l.df
                            %>% filter(risk %in% c("MSM", "MSM & IDU"))
                            , aes(x=Simulation, y=reduce, fill=Simulation)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  #geom_line(position=pd) +
  #geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("") +
  ylab("Reduction in New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Sim (E)", "Sim (E+G)"),
                   labels=c("Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Reduction in New Infections \nStratified by Transmission Risk: Intervention \n(Increase in PrEP among MSM)") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 1, scales = "free")

fig_inf_reduce_int

svglite(filename = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/fig_inf_time_epi_epigenetic_reduce_int.svg",
        width = 10, height = 5)
fig_inf_reduce_int
dev.off()

fig_inf_reduce_int = ggplot(inf_time_demo_sum_mean_l.df, aes(x=year, y=reduce, colour=Simulation, group=Simulation)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Reduction in New Infections") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Sim (E)", "Sim (E+G)"),
                   labels=c("Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Reduction in New Infections \nStratified by Transmission Risk: Intervention \n(Increase in PrEP among MSM)") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_inf_reduce_int

svglite(filename = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/fig_inf_time_epi_epigenetic_reduce_int_time.svg",
        width = 10, height = 5)
fig_inf_reduce_int
dev.off()


###################

diag_time_demo_sum_mean.df = diag_time_demo_sum_all.df %>%
  group_by(risk, year, sim_type, input_file) %>%
  summarize(mean_diag = mean(tot_diag),
            sd_diag = sd(tot_diag)) %>%
  mutate(type = "Simulation")

diag_time_demo_sum_plot.df = bind_rows(diag_time_demo_sum_mean.df,
                                       SD_diag.df) %>%
  mutate(risk = case_when(risk == "IDU" ~ "IDU",
                          risk == "MSM" ~ "MSM",
                          risk == "MSMandIDU" ~ "MSM & IDU",
                          risk == "other" ~ "Other"))

pd <- position_dodge(0.1) # move them .05 to the left and right

diag_time_demo_sum_plot_TEMP.df = diag_time_demo_sum_plot.df %>%
  filter(sim_type %in% c("Observed", "epi_genetic", "epi"))

#####Slide 34a

fig_diag_time = ggplot(diag_time_demo_sum_plot_TEMP.df, aes(x=year, y=mean_diag, colour=sim_type, group=input_file)) +
  geom_errorbar(aes(ymin=mean_diag-1.96*sd_diag, ymax=mean_diag+1.96*sd_diag), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Year") +
  ylab("Number of New Diagnoses") +
  scale_colour_hue(name="Data",    # Legend label, use darker colors
                   #breaks=c("Observed", "epi", "epi_genetic", "epi_int", "epi_genetic_int"),
                   #labels=c("Obs", "Sim (E)", "Sim (E+G)", "Sim Int (E)", "Sim Int (E+G)"),
                   breaks=c("Observed", "epi", "epi_genetic"),
                   labels=c("Obs", "Sim (E)", "Sim (E+G)"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Number of New Diagnoses Over Time\nStratified by Transmission Risk") +
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:30*10) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        #legend.position=c(1,0)
        legend.position = "bottom") +
  facet_wrap(~risk, nrow = 2, scales = "free")

fig_diag_time

svglite(filename = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/Results_manuscript/Results/fig_diag_time_epi_epigenetic_all.svg",
        width = 10, height = 5)
fig_diag_time
dev.off()

###################

diag_time_demo_sum_mean.df = diag_time_demo_sum_all.df %>%
  group_by(risk, year, sim_type, input_file) %>%
  summarize(mean_diag = mean(tot_diag),
            sd_diag = sd(tot_diag)) %>%
  ungroup() %>%
  group_by(year, sim_type, input_file) %>%
  summarize(tot_year = sum(mean_diag)) %>%
  mutate(type = "Simulation")

###################
###################
###################

simData <- inflate_module(simData, simObj$inflation)
simDataDisc <- discount_module(simData, simObj$discount)

simData <- list(notdisc = simData,
                disc    = simDataDisc)

trans_tree.df = bind_rows(trans_tree.df %>%
                            mutate(ID2 = as.character(ID2)),
                          simObj$trans_tree %>%
                            mutate(ID1 = as.character(ID1),
                                   ID2 = as.character(ID2)))


#####
simObj$trans_tree %>% nrow()
x = simObj$trans_tree %>% group_by(month) %>% summarize(infects = n())
plot(x)

trans_tree_demo.df = left_join(simObj$trans_tree, bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
                                                            simObj$popdf_dead) %>% select(id, risk),
                               by = join_by(ID2 == id))

trans_tree_demo.df = trans_tree_demo.df %>%
  mutate(year = case_when(month %in% c(1:12) ~ 2019,
                          month %in% c(13:24) ~ 2020,
                          month %in% c(25:36) ~ 2021))

trans_tree_demo.df %>%
  group_by(month, risk, year) %>%
  summarize(infects = n()) %>%
  ungroup() %>%
  group_by(risk, year) %>%
  summarize(tot_infects = sum(infects)) %>%
  mutate(per = tot_infects/(simObj$trans_tree %>% nrow()))

#####

#saveRDS(simData, "../results/rw_216months_7_26_22.rds")
#saveRDS(simObj, "../results/rw_216months_7_26_22_simObj.rds")

node_list.df = bind_rows(simObj$popdf %>% select(id, gender, risk, age, race),
          simObj$popdf_dead) %>%
  mutate(gender_short = case_when(gender == "female" ~ "F",
                                  gender == "male" ~ "M")) %>%
  mutate(shape = case_when(gender == "female" ~ "square",
                                  gender == "male" ~ "diamond")) %>%
  mutate(race_short = case_when(race == "black" ~ "B",
                                race == "other" ~ "O",
                                race == "hispanic" ~ "H")) %>%
  mutate(risk_short = case_when(risk == "IDU" ~ "I",
                                risk == "MSM" ~ "M",
                                risk == "other" ~ "N",
                                risk == "MSMandIDU" ~ "B")) %>%
  mutate(color = case_when(risk == "IDU" ~ "orange",
                                risk == "MSM" ~ "green",
                                risk == "other" ~ "red",
                                risk == "MSMandIDU" ~ "black")) %>%
  unite("group", c(gender_short, risk_short, race_short), sep= "", remove = TRUE) %>%
  select(id, group, shape, color)

edge_list.df = simObj$trans_tree %>%
  rename(from = ID1,
         to = ID2)



edge_list.df$width <- 1+links$weight/8 # line width
edge_list.df$color <- "gray"    # line color
edge_list.df$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
edge_list.df$smooth <- FALSE    # should the edges be curved?
edge_list.df$shadow <- FALSE    # edge shadow

#install.packages('visNetwork')
library('visNetwork')

visnet <- visNetwork(node_list.df, edge_list.df)
visnet
visLegend(visnet, main="Legend", position="right", ncol=4)


#write_tsv(trans_tree.df,
#          file = "../results/rw_120months_7_11_22_trans_tree.tsv",
#          col_names = FALSE)

#write_tsv(simObj$diag_time,
#          file = "../results/rw_120months_7_11_22_diag_time.tsv",
#          col_names = FALSE)

#  popdf_all = bind_rows(simObj$popdf_dead %>% select(id, gender, risk, age, race),
#                        simObj$popdf %>% select(id, gender, risk, age, race))
#  diag_time = left_join(simObj$diag_time %>% filter(event == "diagnosis"),
#                        popdf_all,
#                        by = c("ID" = "id"))
#  diag_time = diag_time %>%
#    rename(age_at_end = age)
#
#  write_csv(diag_time,
#            file = "../results/SIM_diag_time_7_26_22.csv")
#
# diag_time = diag_time %>% mutate(enter_month = (age_at_end - 13)*12)
# diag_time = diag_time %>% mutate(diag_age = age_at_end - month/12)
#

