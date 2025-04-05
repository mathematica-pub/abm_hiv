
migration <- function(simObj) {

  num_migrationin = simObj$migrationin
  num_migrationout = simObj$migrationout

  #----Migration IN
  TEMP = simObj$geomigrationin %>%
    pivot_longer(
      cols = starts_with("migrate_in_R"),
      names_to = "geo",
      names_prefix = "migrate_in_R",
      values_to = "migration_prob",
      values_drop_na = FALSE
    ) %>%
    mutate(geo = as.integer(geo))  %>%
    mutate(migration_prob = migration_prob + 0.1)

  popgeo.df = left_join(simObj$popdf, TEMP,
                        by = join_by(agegroup, gender, race, risk, stage, geo)) %>%
    mutate(migration_prob = replace_na(migration_prob, 0))

  migration_in.df = sample_n(popgeo.df,
                          size =  rpois(n=1, lambda = num_migrationin),
                          replace = TRUE,
                          weight = migration_prob) %>%
    select(colnames(simObj$popdf))

  max_id = max(simObj$popdf$id, simObj$negpopdf$id)
  migration_in.df$id = c((max_id+1):(max_id+nrow(migration_in.df)))

  simObj$popdf = bind_rows(simObj$popdf, migration_in.df)

  #Add to transmission tree

  trans_tree_migration.df <- tibble(ID1 = "None",
                                    ID2 = migration_in.df$id,
                                    month = simObj$month)

  simObj$notrans_tree.df = bind_rows(simObj$notrans_tree.df,
                            trans_tree_migration.df)

  # Add to networks

  #Happens over time

  #----Migration Out

  TEMP = simObj$geomigrationout %>%
    pivot_longer(
      cols = starts_with("migrate_out_R"),
      names_to = "geo",
      names_prefix = "migrate_out_R",
      values_to = "migration_prob",
      values_drop_na = FALSE
    ) %>%
    mutate(geo = as.integer(geo)) %>%
    mutate(migration_prob = migration_prob + 0.1)

  popgeo.df = left_join(simObj$popdf, TEMP,
                        by = join_by(agegroup, gender, race, risk, stage, geo)) %>%
    mutate(migration_prob = replace_na(migration_prob, 0))

  popgeo.df = sample_n(popgeo.df,
           size = rpois(n=1, lambda = num_migrationout),
           replace = FALSE,
           weight = migration_prob) %>%
    select(colnames(simObj$popdf))

  simObj$popdf_migrate = bind_rows(simObj$popdf_migrate,
                                   popgeo.df %>% select(id, gender, risk, age, race, geo))

  simObj$popdf = simObj$popdf %>%
    filter(id %!in% popgeo.df$id)

  #Remove from networks
  simObj$networks$S_MSM_Net <- simObj$networks$S_MSM_Net %>% filter(ID2 %!in% popgeo.df$id)
  simObj$networks$S_nonMSM_Net <- simObj$networks$S_nonMSM_Net %>% filter(ID2 %!in% popgeo.df$id)
  simObj$networks$IDU_Net <- simObj$networks$IDU_Net %>% filter(ID2 %!in% popgeo.df$id)

  simObj$networks <- lapply(simObj$networks, filter, ID1 %!in% popgeo.df$id)

  simObj$networks$MSM <- simObj$networks$MSM %>%
    filter(ID2 %!in% popgeo.df$id)
  simObj$networks$HET <- simObj$networks$HET %>%
    filter(ID2 %!in% popgeo.df$id)
  simObj$networks$IDU <- simObj$networks$IDU %>%
    filter(ID2 %!in% popgeo.df$id)

  #Within health district migration

  migration_within.df = slice_sample(simObj$popdf, prop = simObj$migrationwithin) %>%
    select(id, stage, agegroup, gender, race, risk, geo) %>%
    left_join(simObj$georeside,
              by = join_by(stage, agegroup, gender, race, risk),
              relationship = "many-to-one") %>%
    mutate(across(starts_with("reside_R"), ~ . + 0.1))

  sample_TEMP <- function(x) {
    geo_loc = x[1]
    x = x[-1]
    x[geo_loc] = 0
    if (any(is.na(x))) {
      sample(x = c(1:13),
             size = 1,
             replace = FALSE)
    } else {
      sample(x = c(1:13),
             size = 1,
             replace = FALSE,
             prob = x)
    }

  }

  migration_within.df$new_geo = apply(migration_within.df %>% select(geo, reside_R1:reside_R13),
        1,
        sample_TEMP)

  simObj$popdf = left_join(simObj$popdf,
                           migration_within.df %>%
                             select(id, new_geo),
                           by = join_by(id),
                           relationship = "one-to-one") %>%
    mutate(new_geo_2 = coalesce(new_geo, geo))

  #partnerships

  partnerships_posneg.df = bind_rows(bind_rows(simObj$networks$S_MSM_Net,
                      simObj$networks$S_nonMSM_Net),
            simObj$networks$IDU_Net) %>%
    filter(ID1 %in% migration_within.df$id) %>%
    left_join(simObj$popdf %>%
                select(id, new_geo, geo),
              by = join_by(ID1 == id)) %>%
    left_join(simObj$negpopdf %>%
                select(id, geo),
              by = join_by(ID2 == id)) %>%
    filter(geo.x == geo.y) %>%
    select(ID2, new_geo) %>%
    group_by(ID2) %>%
    slice_head()

  simObj$negpopdf = simObj$negpopdf %>%
    left_join(partnerships_posneg.df,
              by = join_by(id == ID2),
              relationship = "one-to-one") %>%
    mutate(new_geo = coalesce(new_geo, geo)) %>%
    mutate(geo = new_geo) %>%
    select(-new_geo)

  simObj$popdf = simObj$popdf %>%
      mutate(geo = new_geo_2) %>%
      select(-c(new_geo, new_geo_2))

  return(simObj)
}
