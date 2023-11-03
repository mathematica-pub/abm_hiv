
link_create <- function(file_loc_link, simObj) {


  # •	Will need to change data filter once we figure out how to “stabilize” our population as the data extracts are still changing (sent a note to Gabe/Natasha/Geoff/Alan)
  # •	Need to figure out a better approach to address the missing demographic characteristics
  # UCI vs. UCSD id column
  # Figure out about stage 8

  demographics = read_delim(file_loc_link,
                            delim = ",",
                            col_select = c(UCSD_id,
                                           Stage,
                                           `Stage 2`,
                                           `Stage 3`,
                                           `Stage 4`,
                                           `Stage 5`,
                                           `Stage 6`,
                                           `Stage 7`,
                                           `Diagnosis year`,
                                          agegroup_MT,
                                           gender_MT,
                                           risk_MT,
                                           race_MT))

  demographics = demographics %>%
    filter(Stage %in% c(3,4,5,6))
#  %>%
#    filter(!is.na(agegroup_MT)) %>%
#    filter(!is.na(gender_MT)) %>%
#    filter(!is.na(risk_MT)) %>%
#    filter(!is.na(race_MT))

#  demographics %>% nrow()

  # demographics = read_delim(file_loc_link,
  #                           delim = ",",
  #                           col_select = c(UCI,
  #                                          cur_in_sd_county,
  #                                          alive_1_1_2019,
  #                                          `Diagnosis year`,
  #                                          dob_fmt,
  #                                          race,
  #                                          birth_sex,
  #                                          `Exposure Category`),
  #                           show_col_types = FALSE) %>%
  #   rename(UCSD_id = UCI )

  # demographics = demographics %>% filter(cur_in_sd_county == 1,
  #                                        alive_1_1_2019 == 1,
  #                                        `Diagnosis year` < 2019)

  #seed_seq = read_csv("/Users/ravigoyal/Downloads/seed_tree_IDs.txt",
  #                    col_names = FALSE)
  #temp = intersect(seed_seq$X1, demographics$UCSD_id)

  #link = read_tsv("/Users/ravigoyal/Downloads/tmp/abm_hiv_id_map.tsv")

  #temp = intersect(seed_seq$X1, link_county_abm.df$UCSD_id)
  #temp = intersect(seed_seq$X1, link$UCSD_id)

  #change the labels in the converted string to whatever categories you want.
  # transmission.conversion <- data.frame(raw = c('MSM',
  #                                               'IDU',
  #                                               'MSM & IDU',
  #                                               'Other',
  #                                               'No reported risk',
  #                                               'Heterosexual contact',
  #                                               'Perinatal exposure'),
  #                                       converted = c('MSM',
  #                                                     'IDU',
  #                                                     'MSMandIDU',
  #                                                     'other',
  #                                                     NA,
  #                                                     'other',
  #                                                     'other'))
  #
  # gender.conversion <- data.frame(raw = c('M',
  #                                         'F'),
  #                                 converted = c('male',
  #                                               'female'))
  #
  # #change the labels in the converted string to whatever categories you want.
  # # race.cats <-data.frame(raw = c(1:8),
  # #                        converted = c('Hispanic, any race',
  # #                                               'Not Hispanic, American Indian/Alaska Native',
  # #                                               'Not Hispanic, Asian',
  # #                                               'Not Hispanic, Black',
  # #                                               'Not Hispanic, Native Hawaiian/Pacific Islander',
  # #                                               'Not Hispanic, White',
  # #                                               'Not Hispanic, Legacy Asian/Pacific Islander',
  # #                                               'Not Hispanic, Multi-race'))
  #
  # race.cats <-data.frame(raw = c(1:8),
  #                        converted = c('hispanic',
  #                                      'other',
  #                                      'other',
  #                                      'black',
  #                                      'other',
  #                                      'other',
  #                                      'other',
  #                                      'other'))
  #
  # county_demo.df <- demographics %>% as_tibble %>%
  #   mutate(age = as.numeric(difftime(as.Date("01/01/2019", "%m/%d/%Y"), dob_fmt)/365.25),
  #          agegroup = case_when(age < 24 ~ "youth",
  #                               age < 55 ~ "adult",
  #                               TRUE ~ "olderadult"),
  #          race = as.character(factor(race,levels = race.cats$raw,
  #                                     labels = race.cats$converted)),
  #          gender = as.character(factor(birth_sex ,levels = gender.conversion$raw,
  #                                       labels = gender.conversion$converted)),
  #          risk = as.character(factor(`Exposure Category`,levels = transmission.conversion$raw,
  #                                     labels = transmission.conversion$converted))) %>%
  #   select(UCSD_id, agegroup, gender, race, risk)

  #table(county_demo.df$risk, useNA = "ifany")
  #table(county_demo.df$gender, useNA = "ifany")

  diag_init_cov.df <- simObj$popdf %>%
    filter(stage %in% c("suppress", "left", "diag", "care", "dead")) %>%
    select(id, agegroup, gender, race, risk)

  county_demo.df <- demographics %>%
    rename(agegroup = agegroup_MT,
           gender = gender_MT,
           race = race_MT,
           risk = risk_MT)

  county_demo.df = county_demo.df %>%
    mutate(gender = case_when(
      gender == "Male" ~ "male",
      gender == "Female" ~ "female",
              .default = NA))

  county_demo.df = county_demo.df %>%
    mutate(agegroup = case_when(
      agegroup == "Older Adult (55-100)" ~ "olderadult",
      agegroup == "Adult (25-54)" ~ "adult",
      agegroup == "Youth (13-24)" ~ "youth",
      .default = NA))

  county_demo.df = county_demo.df %>%
    mutate(risk = case_when(
      risk == "MSM" ~ "MSM",
      risk == "other" ~ "other",
      risk == "MSMandIDU" ~ "MSMandIDU",
      risk == "IDU" ~ "IDU",
      .default = NA))

  county_demo_lim.df = county_demo.df %>%
    rename(Diagnosis_year = `Diagnosis year`) %>%
    mutate(risk = as_factor(risk),
           agegroup = as_factor(agegroup),
           gender = as_factor(gender),
           race = as_factor(race)) %>%
    select(-c(`Stage 2`, `Stage 3`, `Stage 4`, `Stage 5`, `Stage 6`, `Stage 7`)) #, `Diagnosis year`

  county_demo_imp.df = mice(county_demo_lim.df, meth = c("","","", "pmm", "pmm", "pmm","pmm"), m = 1)

  county_demo.df = complete(county_demo_imp.df,1)

  #sapply(county_demo_temp.df, function(x) sum(is.na(x)))

  #agegroup_i = "olderadult"
  #gender_i = "male"
  #race_i = "other"
  #risk_i = "MSM"

  link_county_abm.df = tibble(abm_id = NULL,
                              UCSD_id = NULL)

  test.df = tibble(agegroup = NULL,
                   gender = NULL,
                   race = NULL,
                   risk = NULL,
                   ABM = NULL,
                   Obs = NULL,
                   Diff = NULL)

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

          test_i.df = tibble(agegroup = agegroup_i,
                           gender = gender_i,
                           race = race_i,
                           risk = risk_i,
                           ABM = nrow(diag_init_cov_i.df),
                           Obs = nrow(county_demo_i.df),
                           Diff = nrow(diag_init_cov_i.df) - nrow(county_demo_i.df))

          test.df = bind_rows(test.df, test_i.df)

          if (nrow(county_demo_i.df) >= nrow(diag_init_cov_i.df)) {
            link_county_abm_i.df = tibble(abm_id = diag_init_cov_i.df$id,
                                          UCSD_id = sample(x = county_demo_i.df$UCSD_id, size = nrow(diag_init_cov_i.df), replace = FALSE))
          } else {
            link_county_abm_i.df = tibble(abm_id = diag_init_cov_i.df$id,
                                          UCSD_id = c(county_demo_i.df$UCSD_id, rep(NA, nrow(diag_init_cov_i.df)-nrow(county_demo_i.df))))
            #print(paste(nrow(diag_init_cov_i.df)-nrow(county_demo_i.df), agegroup_i, gender_i, race_i, risk_i))
          }

          link_county_abm.df = bind_rows(link_county_abm.df, link_county_abm_i.df)
        }
      }
    }
  }


  return(link_county_abm.df)
}
