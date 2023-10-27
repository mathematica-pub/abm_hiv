
transmission_module <- function(simObj) {

  if (simObj$testflag) check_assert_that(simObj,
                                         New_infection.df = NULL,
                                         checks =c(1:9))

  New_infection_ID_IDU =
    transmission_module_spread_HIV(simObj,
                                   Net_name = "IDU_Net",
                                   trans_prob = simObj$trans_params$trans_prob_IDU)
  New_infection_ID_MSM =
    transmission_module_spread_HIV(simObj, Net_name = "S_MSM_Net",
                                   trans_prob = simObj$trans_params$trans_prob_RNA_MSM)
  New_infection_ID_nonMSM =
    transmission_module_spread_HIV(simObj, Net_name = "S_nonMSM_Net",
                                   trans_prob = simObj$trans_params$trans_prob_RNA_nonMSM)

  unique_id = unique(c(New_infection_ID_IDU, New_infection_ID_MSM, New_infection_ID_nonMSM))

  if (length(unique_id) > 0) {

    New_infection.df = data.frame(id = unique_id,
                                  risk = "other",
                                  gender = "female",
                                  stringsAsFactors = FALSE)

    New_infection.df$gender[New_infection.df$id %in% simObj$ids$males] = "male"
    New_infection.df$risk[New_infection.df$id %in% simObj$ids$msm] = "MSM"
    New_infection.df$risk[New_infection.df$id %in% simObj$ids$idu] = "IDU"
    New_infection.df$risk[New_infection.df$id %in% intersect(simObj$ids$msm, simObj$ids$idu)] = "MSMandIDU"

    IDU_notMSM = setdiff(simObj$ids$idu, simObj$ids$msm)
    MSM_noIDU = setdiff(simObj$ids$msm, simObj$ids$idu)
    MSM_IDU = intersect(simObj$ids$msm, simObj$ids$idu)
    nonMSM_noIDU = setdiff(simObj$ids$notmsm, simObj$ids$idu)

    IDU_notMSM_neg = setdiff(IDU_notMSM, simObj$popdf$id)
    MSM_noIDU_neg = setdiff(MSM_noIDU, simObj$popdf$id)
    MSM_IDU_neg = setdiff(MSM_IDU, simObj$popdf$id)
    nonMSM_noIDU_neg = setdiff(nonMSM_noIDU, simObj$popdf$id)

    simObj = transmission_module_update_simObj(simObj, New_infection.df)

    simObj$networks <- lapply(simObj$networks, filter, ID2 %!in% simObj$popdf$id)

    if (simObj$testflag) check_assert_that(simObj,
                                           New_infection.df = NULL,
                                           checks =c(1:9))

    simObj = transmission_module_new_infection(simObj, New_infection.df)

    if (simObj$testflag) check_assert_that(simObj,
                                           New_infection.df = NULL,
                                           checks =c(1:9))
  }

  simObj = transmission_module_evolve_Net(simObj = simObj, Net_name = "S_MSM_Net")
  simObj = transmission_module_evolve_Net(simObj = simObj, Net_name = "S_nonMSM_Net")

  if (simObj$testflag) check_assert_that(simObj,
                                         New_infection.df = NULL,
                                         checks =c(1:9))

  simObj = transmission_module_pop_growth(simObj)

  return(simObj)
}




