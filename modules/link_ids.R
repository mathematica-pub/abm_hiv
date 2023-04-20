
link_create <- function(file_loc_link, simObj) {


  # •	Will need to change data filter once we figure out how to “stabilize” our population as the data extracts are still changing (sent a note to Gabe/Natasha/Geoff/Alan)
  # •	Need to figure out a better approach to address the missing demographic characteristics
  # UCI vs. UCSD id column
  # Figure out about stage 8

  demographics = read_csv(file_loc_link)

  #%>%
  #  rename(UCSD_id = UCI )

  demographics = demographics %>% filter(cur_in_sd_county == 1,
                          alive_1_1_2019 == 1,
                          `Diagnosis year` < 2019)

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


  # agegroup_i = "youth"
  # gender_i = "male"
  # race_i = "other"
  # risk_i = "MSM"

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

  return(link_county_abm.df)
}
