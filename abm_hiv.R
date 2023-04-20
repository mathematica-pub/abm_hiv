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

file_loc_input = "C:/Users/ravij/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/RWHAP_Equity-feat-add_equity_outcomes/inputs_2019/user_inputs_Current_RWHAP - 200K - PrEP.xlsx"

inputObj <- input_module(origin = file_loc_input)

inputObj$testflag <- TRUE
inputObj$valflag  <- FALSE

set.seed(inputObj$seed)

tic()
simObj   <- initialization_module(inputObj)
toc()

simObj   <- initialize_prep(simObj,
                            origin = file_loc_input)

trans_tree.df <- tibble(ID1 = "None",
                        ID2 = simObj$popdf$id,
                        month = 0)

simObj$trans_tree <- tibble(ID1 = integer(),
                            ID2 = integer(),
                            month = integer())

simObj$diag_time <- tibble(ID = simObj$popdf %>%
                             filter(stage %in% c("suppress", "left", "diag", "care", "dead")) %>%
                             pull(id),
                           month = 0,
                           event = "initial")

####
library(lubridate)
demographics = read_csv("C:/Users/ravij/Dropbox/Academic/Research/Projects/HRSA_SanDiego_modeling/SD_data/sd_county_demographics.csv") %>%
  rename(UCSD_id = UCI )

# demographics %>% filter(cur_in_sd_county == 1,
#                         alive_1_1_2019 == 1,
#                         `Diagnosis year` < 2019
# ) %>% nrow()
#
# demographics %>% filter(cur_county == "San Diego County",
#                         alive_1_1_2019 == 1,
#                         `Diagnosis year` < 2019
# ) %>% nrow()
#
#
#
# demographics %>% filter(cur_in_sd_county == 1,
#                         alive_1_1_2019 == 1,
#                         `Diagnosis year` < 2019,
#                         stage %in% c(3,4,5,6,7)
#                         ) %>% nrow()
#
# demographics %>% filter(stage %in% c(3,4,5,6,7,8)) %>% nrow()


#change the labels in the converted string to whatever categories you want.
transmission.conversion <- data.frame(raw = c('MSM',
                                              'IDU',
                                              'MSM & IDU',
                                              'Other',
                                              'No reported risk',
                                              'Heterosexual contact',
                                              'Perinatal exposure'),
                                      converted = c('MSM',
                                                    'IDU',
                                                    'MSMandIDU',
                                                    'other',
                                                    'other',
                                                    'other',
                                                    'other'))

gender.conversion <- data.frame(raw = c('Male',
                                        'Female',
                                        'Additional Gender Identity',
                                        'Transgender-- Female to male',
                                        'Transgender-- Male to female'),
                                converted = c('male',
                                              'female',
                                              'male',
                                              'male',
                                              'female'))

#change the labels in the converted string to whatever categories you want.
# race.cats <-data.frame(raw = c(1:8),
#                        converted = c('Hispanic, any race',
#                                               'Not Hispanic, American Indian/Alaska Native',
#                                               'Not Hispanic, Asian',
#                                               'Not Hispanic, Black',
#                                               'Not Hispanic, Native Hawaiian/Pacific Islander',
#                                               'Not Hispanic, White',
#                                               'Not Hispanic, Legacy Asian/Pacific Islander',
#                                               'Not Hispanic, Multi-race'))

race.cats <-data.frame(raw = c(1:8),
                       converted = c('Hispanic',
                                     'other',
                                     'other',
                                     'black',
                                     'other',
                                     'other',
                                     'other',
                                     'other'))

county_demo.df <- demographics %>% as_tibble %>%
  mutate(age = as.numeric(difftime(as.Date("01/01/2019", "%m/%d/%Y"), dob_fmt)/365.25),
         agegroup = case_when(age < 24 ~ "youth",
                              age < 55 ~ "adult",
                              TRUE ~ "olderadult"),
         race = as.character(factor(race,levels = race.cats$raw,
                                           labels = race.cats$converted)),
         gender = as.character(factor(`Current gender`,levels = gender.conversion$raw,
                                             labels = gender.conversion$converted)),
         risk = as.character(factor(`Exposure Category`,levels = transmission.conversion$raw,
                                                          labels = transmission.conversion$converted))) %>%
  select(UCSD_id, agegroup, gender, race, risk)

diag_init_cov.df <- simObj$popdf %>%
  filter(stage %in% c("suppress", "left", "diag", "care", "dead")) %>%
  select(id, agegroup, gender, race, risk)


agegroup_i = "youth"
gender_i = "male"
race_i = "other"
risk_i = "MSM"

link_county_abm.df = tibble(abm_id = NULL,
                               UCSD_id = NULL)

for (agegroup_i in (diag_init_cov.df$agegroup %>% unique())) {
  for (gender_i in (diag_init_cov.df$gender %>% unique())) {
    for (race_i in  (diag_init_cov.df$race %>% unique())) {
      for (risk_i in  (diag_init_cov.df$risk %>% unique())) {

        diag_init_cov_i.df <- diag_init_cov.df %>%
          filter(agegroup == agegroup_i,
                 gender == gender_i,
                 race == race_i,
                 risk == risk_i)

        county_demo_i.df <- county_demo.df %>%
          filter(agegroup == agegroup_i,
                 gender == gender_i,
                 race == race_i,
                 risk == risk_i)

        if (nrow(county_demo_i.df) >= nrow(diag_init_cov_i.df)) {
          link_county_abm_i.df = tibble(abm_id = diag_init_cov_i.df$id,
                                      UCSD_id = sample(x = county_demo_i.df$UCSD_id, size = nrow(diag_init_cov_i.df), replace = FALSE))
        } else {
          link_county_abm_i.df = tibble(abm_id = diag_init_cov_i.df$id,
                                           UCSD_id = c(county_demo_i.df$UCSD_id, rep(NA, nrow(diag_init_cov_i.df)-nrow(county_demo_i.df))))
        }

        link_county_abm.df = bind_rows(link_county_abm.df, link_county_abm_i.df)
      }
    }
  }
}

link_county_abm.df

link_county_abm.df %>% filter(is.na(UCSD_id)) %>% nrow()

####

simData <- data.frame(list())
for (i in 1:simObj$duration) {
  tic()
  print(i)
  simObj <- increment_module(simObj)
  simObj <- transmission_module(simObj)
  simObj <- care_stage_module(simObj)
  simObj <- health_state_module(simObj)
  simObj <- outcomes_module(simObj)
  simObj <- prep_update(simObj)
  simData <- bind_rows(simData, collapse_module(simObj))
  toc()
}

simData <- inflate_module(simData, simObj$inflation)
simDataDisc <- discount_module(simData, simObj$discount)

simData <- list(notdisc = simData,
                disc    = simDataDisc)

trans_tree.df = bind_rows(trans_tree.df %>%
                            mutate(ID2 = as.character(ID2)),
                          simObj$trans_tree %>%
                            mutate(ID1 = as.character(ID1),
                                   ID2 = as.character(ID2)))

saveRDS(simData, "../results/rw_216months_7_26_22.rds")
saveRDS(simObj, "../results/rw_216months_7_26_22_simObj.rds")

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

