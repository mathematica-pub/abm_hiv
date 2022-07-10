
#------------------------------------------------------------------------------#
#--- Supporting functions for multiple modules
#------------------------------------------------------------------------------#

yr_mon_range  <- function(year) return(((year-1)*12+1):(year*12))
yr_mon_ranges <- function(years) return(flatten_int(lapply(years, yr_mon_range)))

gen_agegroup <- function(data) {
  data %>%
    mutate(agegroup = case_when(13 <= age & age < 25   ~ "youth",
                                25 <= age & age < 55   ~ "adult",
                                55 <= age              ~ "olderadult"))
}

gen_age <- function(data) {
  data %>%
    mutate(age = case_when(agegroup == "youth"      ~ runif(nrow(.), 13, 25),
                           agegroup == "adult"      ~ runif(nrow(.), 25, 55),
                           agegroup == "olderadult" ~ runif(nrow(.), 55, 100)))
}

'%!in%' <- function(x,y)!('%in%'(x,y))

MPRrcat <- function(n, p) {
  #Based on LaplacesDemon implementation, which has bugs and no error checking.
  #p is a matrix n x k of probabilities
  #Make probabilities sum to 1 if they don't
  sums <- rowSums(p)
  if (!isTRUE(all.equal(sums, rep(1, length(sums)),
                        tolerance = .Machine$double.eps))) {
    warning(paste0("Some class probabilities do not sum to 1. ",
                   "In such cases, the probabilities will be normalized to sum to 1."))
    p <- t(apply(p, 1, function(x) x/sum(x)))
  }
  d <- dim(p)
  n <- d[1]
  k <- d[2]
  lev <- dimnames(p)[[2]]
  if (!length(lev))
    lev <- 1:k
  U <- apply(p, 1, cumsum)
  un <- rep(runif(n), rep(k, n))
  x <- lev[1 + colSums(un > U)]
}

#------------------------------------------------------------------------------#
#--- Following Functions used primarily by transmission module but also during
#--- the input or initialization phase.
#------------------------------------------------------------------------------#

#to form network links by risk
gen_risk_net <- function(simObj,
                         risk,
                         newData = simObj$popdf,
                         oldData = simObj$popdf) {
  risk <- toupper(risk)

  HIVpos_ID <- newData %>% filter(risk %in% c(!!risk, "MSMandIDU")) %>% pull(id)

  if (isTRUE(all.equal(newData, oldData))) {
    HIVneg_ID <- setdiff(simObj$ids[[tolower(risk)]], HIVpos_ID)
  } else {
    HIVneg_ID <- setdiff(simObj$ids[[tolower(risk)]],
                         oldData %>%
                           filter(risk %in% c(!!risk, "MSMandIDU")) %>%
                           pull(id))
  }

  if (risk == "IDU") {
    discordant_mult = simObj$trans_params$discordant_IDU
  } else if(risk == "MSM") {
    discordant_mult = simObj$trans_params$discordant_MSM
  }

  transmission_module_create_Net(
    Poisson_mean = simObj$trans_params[[paste0("Poisson_mean_", risk)]],
    HIVpos_ID    = HIVpos_ID,
    HIVneg_ID    = HIVneg_ID,
    type = risk,
    discordant_mult = discordant_mult
    )
}

#to form network links between genders
gen_gender_net <- function(simObj,
                           gender,
                           newData = simObj$popdf,
                           oldData = simObj$popdf) {
  gender <- tolower(gender)

  if      (gender == "male") {
    ogen    <- "female"
    setfunc <- setdiff
  } else if (gender == "female") {
    ogen    <- "male"
    setfunc <- intersect
  }

  get_gendered_ids <- function(data, gender) {
    data %>%
      filter(risk %in% c("other", "IDU") & gender == !!gender) %>%
      pull(id)
  }

  HIVpos_ID <- get_gendered_ids(newData, gender)
  HIVneg_ID <- setdiff(setfunc(simObj$ids$notmsm,
                               simObj$ids$males),
                       get_gendered_ids(oldData, ogen))

  transmission_module_create_Net(
    Poisson_mean = simObj$trans_params$Poisson_mean_nonMSM,
    HIVpos_ID    = HIVpos_ID,
    HIVneg_ID    = HIVneg_ID,
    type = "gender",
    discordant_mult = simObj$trans_params$discordant_nonMSM)
}

transmission_module_create_Net <- function(Poisson_mean, HIVpos_ID, HIVneg_ID, type, discordant_mult) {

  Num_P_HIVneg = discordant_mult * Poisson_mean * (length(HIVneg_ID))/(length(HIVneg_ID) + length(HIVpos_ID))


  Num_partners_HIVneg = rpois(n = length(HIVpos_ID), lambda = Num_P_HIVneg)

  Partners_HIVneg = sample(HIVneg_ID, sum(Num_partners_HIVneg), replace = TRUE)

  HIVneg_ID_rep = flatten_int(mapply(rep, HIVpos_ID, Num_partners_HIVneg,
                                     SIMPLIFY = FALSE))

  Net = data.frame(ID1 = c(HIVneg_ID_rep),
                   ID2 = c(Partners_HIVneg))
  return(Net)
}

check_assert_that <- function(simObj, New_infection.df,
                              checks) {

  #Check correct ID1
  if(1 %in% checks) {
    assert_that(all(simObj$networks$IDU_Net$ID1 %in%
                      (simObj$popdf %>%
                         filter(risk == "IDU" | risk == "MSMandIDU") %>%
                         pull(id))),
                msg = "Error - Transmission Module: IDU network has non-positive or non-IDU IDs in ID1")
  }
  if(2 %in% checks) {
    assert_that(all(simObj$networks$S_MSM_Net$ID1 %in%
                      (simObj$popdf %>%
                         filter(risk == "MSM" | risk == "MSMandIDU") %>%
                         pull(id))),
                msg = "Error - Transmission Module:  MSM network has non-positive or non-MSM IDs in ID1")
  }
  if(3 %in% checks) {
    assert_that(all(simObj$networks$S_nonMSM_Net$ID1 %in%
                      (simObj$popdf %>%
                         filter(risk == "other" | risk == "IDU") %>%
                         pull(id))),
                msg = "Error - Transmission Module: nonMSM network has non-positive or non-nonMSM IDs in ID1")
  }

  #Check ID2 is negative
  if(4 %in% checks) {
    assert_that(!any(simObj$networks$IDU_Net$ID2 %in%
                      (simObj$popdf %>% pull(id))),
                msg = "Error - Transmission Module: IDU network has positive IDs in ID2")
  }
  if(5 %in% checks) {
    assert_that(!any(simObj$networks$S_MSM_Net$ID2 %in%
                       (simObj$popdf %>% pull(id))),
                msg = "Error - Transmission Module: MSM network has positive IDs in ID2")
  }
  if(6 %in% checks) {
    assert_that(!any(simObj$networks$S_nonMSM_Net$ID2 %in%
                       (simObj$popdf %>% pull(id))),
                msg = "Error - Transmission Module: nonMSM network has positive IDs in ID2")
  }

  #Check correct ID2 group
  if(7 %in% checks) {
    assert_that(all(simObj$networks$IDU_Net$ID2 %in% simObj$ids$idu),
                msg = "Error - Transmission Module: IDU network has non-IDU IDs in ID2")
  }
  if(8 %in% checks) {
    assert_that(all(simObj$networks$S_MSM_Net$ID2 %in% simObj$ids$msm),
                msg = "Error - Transmission Module: MSM network has non-MSM IDs in ID2")
  }
  if(9 %in% checks) {
    assert_that(all(simObj$networks$S_nonMSM_Net$ID2 %in% simObj$ids$notmsm),
                msg = "Error - Transmission Module: nonMSM network has non-nonMSM IDs in ID2")
  }
}

#------------------------------------------------------------------------------#
#--- Following function used for inflate and discount modules
#------------------------------------------------------------------------------#

select_order_output_data <- function(data) {
  data %>%
    select(month, rwhap, stage, agegroup, gender, race, risk, contains("cost"),
           qaly, ly, newdeaths, newinfects, pospopsize, negpopsize,
           popdenom, discounted) %>%
    arrange(month, rwhap, stage, agegroup, gender, race, risk)
}
