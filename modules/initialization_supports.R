
#------------------------------------------------------------------------------#
#--- Called directly by initialization module
#------------------------------------------------------------------------------#

initialize_negative_pop <- function(preSimObj) {

  #Get and check population dataframe
  df <- preSimObj$popdf
  if (preSimObj$testflag) stopifnot(length(df$id)==max(df$id))

  #Generate distribution of negative population across gender and risk
  negDist <- preSimObj$popprobs$atrisk %>%
    distinct(agegroup, gender, race, risk, atrisk) %>%
    group_by(gender, risk) %>%
    summarize(atrisk = sum(atrisk), .groups = "drop") %>%
    mutate(count = round(atrisk*nrow(df)*preSimObj$negpopulationmult))

  #Generate ids belonging to each of the gender-risk categories
  negIds <- list()
  maxId  <- max(df$id)
  for (g in 1:nrow(negDist)) {
    grp           <- paste(negDist$gender[g], negDist$risk[g], sep = "_")
    negIds[[grp]] <- (maxId+1):(maxId + negDist$count[g])
    #cat("Min: ", maxId + 1, "\tRange: ", maxId + negDist$count[g], "\n")
    maxId         <- maxId + negDist$count[g]
  }

  #Group categories as needed for use in the transmission module
  ids <- list("males"  = c(filter(df, gender == "male") %>% pull(id),
                           negIds[grepl("^male_", names(negIds))] %>%
                             flatten_int() %>% `names<-`(NULL)),
              "idu"    = c(df %>% filter(grepl("IDU", risk)) %>% pull(id),
                           negIds[grepl("IDU", names(negIds))] %>%
                             flatten_int() %>% `names<-`(NULL)),
              "msm"    = c(df %>% filter(grepl("MSM", risk)) %>% pull(id),
                           negIds[grepl("MSM", names(negIds))] %>%
                             flatten_int() %>% `names<-`(NULL)),
              "notmsm" = c(df %>% filter(!grepl("MSM", risk)) %>% pull(id),
                           negIds[!grepl("MSM", names(negIds))] %>%
                             flatten_int() %>% `names<-`(NULL)))

  if (preSimObj$testflag) stopifnot(n_distinct(flatten_int(ids))==
                                      (sum(negDist$count)+nrow(preSimObj$popdf)),
                                    all.equal(n_distinct(flatten_int(ids)),
                                              nrow(preSimObj$popdf)*
                                                (preSimObj$negpopulationmult+1),
                                              tolerance = 5))

  #Adapt negDist to quickly grow population later.
  growthDist <- negDist %>%
    mutate(count = atrisk*nrow(preSimObj$popdf)*(preSimObj$negpopulationmult+1),
           sets  = case_when(gender=="male"   & risk=="MSMandIDU" ~ "set1",
                             gender=="male"   & risk=="MSM"       ~ "set2",
                             gender=="male"   & risk=="IDU"       ~ "set3",
                             gender=="male"   & risk=="other"     ~ "set4",
                             gender=="female" & risk=="IDU"       ~ "set5",
                             gender=="female" & risk=="other"     ~ "set6")) %>%
    select(sets, everything())

  return(c(preSimObj, "ids" = list(ids), "growthdist" = list(growthDist)))
}
