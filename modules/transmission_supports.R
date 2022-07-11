
#------------------------------------------------------------------------------#
#--- Supporting Functions for Transmission Module
#------------------------------------------------------------------------------#

transmission_module_new_infection <- function(simObj, New_infection.df) {

  IDU_Net   <- gen_risk_net(simObj, "IDU", newData = New_infection.df)
  S_MSM_Net <- gen_risk_net(simObj, "MSM", newData = New_infection.df)

  S_nonMSM_F_Net <- gen_gender_net(simObj, "female", newData = New_infection.df)
  S_nonMSM_M_Net <- gen_gender_net(simObj, "male",   newData = New_infection.df)

  simObj$networks <-
    list(IDU_Net      = bind_rows(simObj$networks$IDU_Net,      IDU_Net),
         S_MSM_Net    = bind_rows(simObj$networks$S_MSM_Net,    S_MSM_Net),
         S_nonMSM_Net = bind_rows(simObj$networks$S_nonMSM_Net,
                                  S_nonMSM_F_Net,
                                  S_nonMSM_M_Net))

  return(simObj)
}

transmission_module_spread_HIV <- function(simObj, Net_name, trans_prob) {

  if (Net_name %in% c("S_MSM_Net", "S_nonMSM_Net")) {
    simObj$networks[[Net_name]] <- left_join(simObj$network[[Net_name]],
                                             select(simObj$popdf, id, rna),
                                             by = c("ID1" = "id"))
    simObj$networks[[Net_name]] <- left_join(simObj$network[[Net_name]],
                                             trans_prob,
                                             by = "rna")
    simObj$networks[[Net_name]] <- select(simObj$networks[[Net_name]], -rna)
  } else {
    simObj$networks[[Net_name]]$trans_prob <- trans_prob
  }

  if (!is.null(simObj$prep_info)) {
    simObj$networks[[Net_name]] <- left_join(simObj$networks[[Net_name]],
                                             simObj$prep_id %>% filter(use == 1) %>% select(id, reduction),
                                             by = c("ID2" = "id")) %>%
      mutate(trans_prob = ifelse(!(is.na(reduction)),trans_prob*reduction, trans_prob)) %>%
      select(-reduction)
  }

  simObj$networks[[Net_name]] <- simObj$networks[[Net_name]] %>%
    mutate(trans_event = runif(nrow(.))<=trans_prob)

  return(simObj$networks[[Net_name]] %>%
           filter(trans_event == 1) %>%
           select(ID1,ID2))

}

transmission_module_evolve_Net <- function(simObj, Net_name) {

  #dissolution

  if (Net_name == "S_MSM_Net") {
    d_param      <- simObj$trans_params$d_param_MSM
  } else if (Net_name == "S_nonMSM_Net") {
    d_param <- simObj$trans_params$d_param_nonMSM
  }

  simObj$networks[[Net_name]] = left_join(simObj$networks[[Net_name]],
                                          select(simObj$popdf, id, agegroup),
                                          by = c("ID1" = "id"))
  simObj$networks[[Net_name]] = left_join(simObj$networks[[Net_name]],
                                          d_param,
                                          by = "agegroup")

  simObj$networks[[Net_name]] <- simObj$networks[[Net_name]] %>%
    mutate(trans_event = runif(nrow(.))<=d_prob)

  simObj$networks[[Net_name]] <-
    simObj$networks[[Net_name]][-which(simObj$networks[[Net_name]]$trans_event == 1),]

  #formation

  if (Net_name == "S_MSM_Net") {
    HIVpos_ID    <- simObj$popdf %>%
      filter(risk == "MSM" | risk == "MSMandIDU") %>%
      pull(id)
    HIVneg_ID    <- setdiff(simObj$ids$msm, HIVpos_ID)
    Poisson_mean <- simObj$trans_params$f_param_MSM
  } else if (Net_name == "S_nonMSM_Net") {
    HIVpos_ID <- simObj$popdf %>%
      filter((risk == "other" | risk == "IDU") & gender == "female") %>%
      pull(id)
    HIVpos_ID_M <- simObj$popdf %>%
      filter((risk == "other" | risk == "IDU") & gender == "male") %>%
      pull(id)
    HIVneg_ID <- setdiff(intersect(simObj$ids$notmsm, simObj$ids$males),
                         HIVpos_ID_M)
    Poisson_mean <- simObj$trans_params$f_param_nonMSM

    HIVpos_ID_2 <- simObj$popdf %>%
      filter((risk == "other" | risk == "IDU") & gender == "male") %>%
      pull(id)
    HIVpos_ID_F <- simObj$popdf %>%
      filter((risk == "other" | risk == "IDU") & gender == "female") %>%
      pull(id)
    HIVneg_ID_2 <- setdiff(setdiff(simObj$ids$notmsm, simObj$ids$males),
                           HIVpos_ID_F)
  }

  formation.df = data.frame(id = HIVpos_ID)
  formation.df = left_join(formation.df,
                           select(simObj$popdf, id, agegroup),
                           by = c("id"))
  formation.df = left_join(formation.df,
                           Poisson_mean,
                           by = "agegroup")

  formation.df$Poisson_mean = formation.df$Poisson_mean  * (length(HIVneg_ID))/(length(HIVneg_ID) + length(HIVpos_ID))

  Num_partners_HIVneg = rpois(n = length(formation.df$id), lambda = formation.df$Poisson_mean)

  Partners_HIVneg = sample(HIVneg_ID, sum(Num_partners_HIVneg), replace = TRUE)
  HIVneg_ID_rep = flatten_int(mapply(rep, HIVpos_ID, Num_partners_HIVneg,
                                     SIMPLIFY = FALSE))

  Net_new = data.frame(ID1 = c(HIVneg_ID_rep),
                       ID2 = c(Partners_HIVneg))

  if (Net_name == "S_nonMSM_Net") {
    formation.df = data.frame(id = HIVpos_ID_2)
    formation.df = left_join(formation.df,
                             select(simObj$popdf, id, agegroup),
                             by = c("id"))
    formation.df = left_join(formation.df,
                             Poisson_mean,
                             by = "agegroup")

    formation.df$Poisson_mean = formation.df$Poisson_mean  * (length(HIVneg_ID_2))/(length(HIVneg_ID_2) + length(HIVpos_ID_2))

    Num_partners_HIVneg = rpois(n = length(formation.df$id), lambda = formation.df$Poisson_mean)

    Partners_HIVneg = sample(HIVneg_ID, sum(Num_partners_HIVneg), replace = TRUE)
    HIVneg_ID_rep = flatten_int(mapply(rep, HIVpos_ID_2, Num_partners_HIVneg,
                                       SIMPLIFY = FALSE))

    Net_new_2 = data.frame(ID1 = c(HIVneg_ID_rep),
                           ID2 = c(Partners_HIVneg))

    Net_new = bind_rows(Net_new, Net_new_2)
  }

  simObj$networks[[Net_name]] <- select(simObj$networks[[Net_name]], -agegroup, -d_prob, -trans_event)
  simObj$networks[[Net_name]] <- bind_rows(simObj$networks[[Net_name]], Net_new)

  return(simObj)

}

transmission_module_update_simObj <- function(simObj, New_infection.df) {

  if (simObj$testflag) {
    stopifnot(!(simObj$month==0))
    stopifnot((pull(simObj$popdf, infectdur) %>% min(na.rm = TRUE)) == 1)
  }

  #Get individual data sets required for building out characteristics of the
  #newly infected.

  #Start with the population distribution between rwhap/non-rwhap
  rwhapProb <- simObj$popprobs$nodes %>%
    filter(stage=="hiv") %>%
    mutate(rwhapstagemult = rwhapstagemult/sum(rwhapstagemult)) %>%
    spread(rwhap, rwhapstagemult)

  #Get distribution by gender risk rwhap
  demoProb <- simObj$popprobs$atrisk %>%
    unite(agerace, agegroup, race) %>%
    spread(agerace, atrisk) %>%
    mutate(rowsums = apply(select(., -gender, -risk, -rwhap), 1,
                           sum, na.rm=TRUE)) %>%
    mutate_at(setdiff(colnames(.), c("gender", "risk", "rwhap")),
              function(x) x/.$rowsums)

  if (simObj$testflag) demoProb <- ensure(demoProb, all(.$rowsums==1))

  demoProb <- select(demoProb, -rowsums)

  #retrieve rnaset for hiv
  rnaProb <- simObj$popprobs$rna %>%
    filter(stage == "hiv") %>%
    spread(rnaset, rnamult)

  if (simObj$testflag) rnaProb <- ensure(rnaProb,
                                         sum(apply(select(., -stage),
                                                   1, sum, na.rm = TRUE))==1)

  #Assign RWHAP to newly infected
  infections <- New_infection.df %>%
    mutate(stage = "hiv") %>%
    left_join(rwhapProb, by = "stage") %>%
    mutate(rwhap = MPRrcat(nrow(.), as.matrix(select(., `TRUE`, `FALSE`))) %>%
             as.logical() %>%
             as.numeric()) %>%
    select(-`FALSE`, -`TRUE`)

  #Assign agegroup and race to newly infected
  infections2 <- infections %>%
    left_join(demoProb, by = c("risk", "gender", "rwhap")) %>%
    mutate(agegroup_race =
             MPRrcat(nrow(.),
                     as.matrix(select(., -id, -risk, -gender, -stage, -rwhap)))) %>%
    select(., id, risk, gender, stage, rwhap, agegroup_race) %>%
    separate(agegroup_race, c("agegroup", "race"))

  #Assign services to newly infected
  infections3 <- infections2 %>%
    left_join(simObj$popprobs$service,
              by = c("risk", "gender", "rwhap", "agegroup", "race")) %>%
    mutate(oahsart = if_else(runif(nrow(.))<=oahsartreceipt, 1, 0)) %>%
    select(-oahsartreceipt)

  services <- c("mcm", "mhsa", "support")
  for (i in services) {
    infections3 <- infections3 %>%
      mutate(!!sym(paste0("bene", i)) :=
               as.numeric(runif(nrow(.)) <= (!!sym(paste0(i, "need")))),
             !!sym(paste0(i, "chk")) :=
               as.numeric(runif(nrow(.)) <= (!!sym(paste0(i, "receipt")))),
             !!i := if_else(!oahsart, 0,
                            (!!sym(paste0("bene", i)))*(!!sym(paste0(i, "chk"))))) %>%
      select_at(vars(-one_of(grep(paste0("^", i, "[a-z]"), colnames(.), value = TRUE))))
  }

  #Assign rnaset, rna, infectmon, infectdur, age to newly infected
  infections5 <- infections3 %>%
    left_join(rnaProb, by = "stage") %>%
    mutate(rnaset = MPRrcat(nrow(.),
                            as.matrix(select_at(., setdiff(colnames(rnaProb),
                                                           "stage")))),
           rna       = "gt50k",
           infectmon = simObj$month,
           infectdur = 0) %>%
    select_at(vars(-one_of(setdiff(colnames(rnaProb), "stage")))) %>%
    gen_age() %>%
    gen_agegroup()

  #Assign laststage, lastsupp to newly infected
	if (simObj$valflag) {
		infections5 <- infections5 %>%
  	  mutate(laststage = "neg",
   	         lastsupp  = NA_integer_)
	}

  #Assign cd4 and precd4 to newly infected
  if (simObj$testflag) stopifnot(simObj$popcd4dists$predist=="Normal")
  infections6 <- infections5 %>%
    left_join(select(simObj$popcd4dists, stage, precd4param1, precd4param2),
              by = "stage") %>%
    mutate(precd4 = rnorm(nrow(.), precd4param1, precd4param2),
           cd4    = precd4) %>%
    select(-precd4param1, -precd4param2)

  #Ensure that everything has been assigned
  if (simObj$testflag) {
    if (simObj$valflag) stopifnot(sum(is.na(select(infections6, -lastsupp)))==0)
    else                stopifnot(sum(is.na(infections6))==0)
    if (!(all(colnames(infections6)  %in% colnames(simObj$popdf)) &&
          all(grep(".*_.*", colnames(simObj$popdf), value = TRUE, invert = TRUE) %in%
              colnames(infections6)))) {
      stop("")
    }
  }

  #Add new infections to the simulation object
  simObj$popdf <- bind_rows(simObj$popdf, infections6)

  return(simObj)
}

transmission_module_pop_growth <- function(simObj) {

  #Apply growth to pre-distributed overall population to see how much to add to
  #each category.
  growthDist <- simObj$growthdist %>%
    mutate(newcount = count*(1+simObj$growthrate),
           growth   = round(newcount - count))

  #Build idsets to add new ids. #gender, MSM, IDU
  idSets <- list(set1 = Reduce(intersect,
                               list(simObj$ids$males, simObj$ids$msm,
                                    simObj$ids$idu)),           # Male, Y, Y
                 set2 = setdiff(intersect(simObj$ids$males, simObj$ids$msm),
                                simObj$ids$idu),                # Male, Y, N
                 set3 = setdiff(intersect(simObj$ids$males, simObj$ids$idu),
                                simObj$ids$msm),                # Male, N, Y
                 set4 = setdiff(simObj$ids$males, c(simObj$ids$msm,
                                                    simObj$ids$idu)), # Male, N, N
                 set5 = setdiff(simObj$ids$idu, simObj$ids$male), # Female, N, Y
                 set6 = setdiff(simObj$ids$notmsm,
                                c(simObj$ids$male, simObj$ids$idu)))  # Female, N, N

  #Apply growth to each id set
  newIdSets <- list()
  for (s in names(idSets)) {
    if   (s=="set1") max_id <- max(c(simObj$ids$notmsm, simObj$ids$msm))
    else             max_id <- max_id + num_id
    num_id <- filter(growthDist, sets==s) %>% pull(growth)
    newIdSets[[s]] <- c((max_id+1):(max_id+num_id))
  }

  simObj$ids <- list(males = c(simObj$ids$males, newIdSets[["set1"]],
                               newIdSets[["set2"]], newIdSets[["set3"]],
                               newIdSets[["set4"]]),
                     msm = c(simObj$ids$msm, newIdSets[["set1"]],
                             newIdSets[["set2"]]),
                     idu = c(simObj$ids$idu, newIdSets[["set1"]],
                             newIdSets[["set3"]], newIdSets[["set5"]]),
                     notmsm = c(simObj$ids$notmsm, newIdSets[["set3"]],
                                newIdSets[["set4"]], newIdSets[["set5"]],
                                newIdSets[["set6"]]))

  #Reset growthdist to reflect the new values in preparation for the following
  #month.
  simObj$growthdist <- growthDist %>%
    mutate(count = newcount) %>%
    select(-newcount, -growth)

  return(simObj)
}
