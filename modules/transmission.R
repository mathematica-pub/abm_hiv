
transmission_module <- function(simObj) {

  if (simObj$testflag) check_assert_that(simObj,
                                         New_infection.df = NULL,
                                         checks =c(1:6))

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

  New_infection_tree = bind_rows(bind_rows(New_infection_ID_IDU, New_infection_ID_MSM),
                                 New_infection_ID_nonMSM) %>%
    group_by(ID1) %>%
    slice_sample(n=1) %>%
    ungroup() %>%
    mutate(month = simObj$month)

  simObj$trans_tree = bind_rows(simObj$trans_tree,
                                New_infection_tree)

  #unique_id = unique(c(New_infection_ID_IDU, New_infection_ID_MSM, New_infection_ID_nonMSM))
  unique_id = New_infection_tree$ID2

  if (length(unique_id) > 0) {

    simObj$popdf = bind_rows(simObj$popdf,
                     simObj$negpopdf %>%
      filter(id %in% unique_id) %>%
      mutate(stage = "hiv",
             rna       = "gt50k",
             infectmon = simObj$month,
             infectdur = 0) %>%
      left_join(select(simObj$popcd4dists, stage, precd4param1, precd4param2),
                by = "stage") %>%
      mutate(precd4 = rnorm(nrow(.), precd4param1, precd4param2),
             cd4    = precd4) %>%
      select(-precd4param1, -precd4param2))

    simObj$negpopdf = simObj$negpopdf %>%
      filter(!(id %in% unique_id))

    # New_infection.df = data.frame(id = unique_id,
    #                               risk = "other",
    #                               gender = "female",
    #                               stringsAsFactors = FALSE)
    #
    # New_infection.df$gender[New_infection.df$id %in% simObj$ids$males] = "male"
    # New_infection.df$risk[New_infection.df$id %in% simObj$ids$msm] = "MSM"
    # New_infection.df$risk[New_infection.df$id %in% simObj$ids$idu] = "IDU"
    # New_infection.df$risk[New_infection.df$id %in% intersect(simObj$ids$msm, simObj$ids$idu)] = "MSMandIDU"

    # IDU_notMSM = setdiff(simObj$ids$idu, simObj$ids$msm)
    # MSM_noIDU = setdiff(simObj$ids$msm, simObj$ids$idu)
    # MSM_IDU = intersect(simObj$ids$msm, simObj$ids$idu)
    # nonMSM_noIDU = setdiff(simObj$ids$notmsm, simObj$ids$idu)
    #
    # IDU_notMSM_neg = setdiff(IDU_notMSM, simObj$popdf$id)
    # MSM_noIDU_neg = setdiff(MSM_noIDU, simObj$popdf$id)
    # MSM_IDU_neg = setdiff(MSM_IDU, simObj$popdf$id)
    # nonMSM_noIDU_neg = setdiff(nonMSM_noIDU, simObj$popdf$id)

    # simObj = transmission_module_update_simObj(simObj, New_infection.df)

    #simObj$networks <- lapply(simObj$networks, filter, ID2 %!in% simObj$popdf$id)

    simObj$networks$S_MSM_Net <- simObj$networks$S_MSM_Net %>% filter(ID2 %!in% simObj$popdf$id)
    simObj$networks$S_nonMSM_Net <- simObj$networks$S_nonMSM_Net %>% filter(ID2 %!in% simObj$popdf$id)
    simObj$networks$IDU_Net <- simObj$networks$IDU_Net %>% filter(ID2 %!in% simObj$popdf$id)

    if (simObj$testflag) check_assert_that(simObj,
                                           New_infection.df = NULL,
                                           checks =c(1:6))

    #simObj = transmission_module_new_infection(simObj, New_infection.df)

    # if (simObj$testflag) check_assert_that(simObj,
    #                                        New_infection.df = NULL,
    #                                        checks =c(1:6))
  }

  simObj = generate_s_net(simObj, init_net=FALSE)

  #simObj = transmission_module_evolve_Net(simObj = simObj, Net_name = "S_MSM_Net")
  #simObj = transmission_module_evolve_Net(simObj = simObj, Net_name = "S_nonMSM_Net")

  if (simObj$testflag) check_assert_that(simObj,
                                         New_infection.df = NULL,
                                         checks =c(1:6))

  #simObj = transmission_module_pop_growth(simObj)

  return(simObj)
}




