initialize_prep <- function(simObj, origin) {

  prep_info = tibble(param = NULL,
                     msm = NULL,
                     idu = NULL,
                     nonmsm = NULL)

  indices = c(4,5,7,8,10,11,12,13,15,16,17,18)
  param_names = c("indication_per",
                  "start_per",
                  "growth_per",
                  "duration_mean",
                  "adherence_per_no",
                  "adherence_per_low",
                  "adherence_per_med",
                  "adherence_per_high",
                  "adherence_reduction_no",
                  "adherence_reduction_low",
                  "adherence_reduction_med",
                  "adherence_reduction_high")

  for (counter in c(1:length(indices))) {

    index = indices[counter]
    param_name = param_names[counter]

    prep_temp = tibble(param = param_name,
                       msm = read_cell(origin, "PrEP",   paste("B",index,sep="")),
                       idu = read_cell(origin, "PrEP",   paste("C",index,sep="")),
                       nonmsm = read_cell(origin, "PrEP",   paste("D",index,sep="")))
    prep_info = bind_rows(prep_info, prep_temp)
  }

  # IDU_notMSM = setdiff(simObj$ids$idu, simObj$ids$msm)
  # MSM_noIDU = setdiff(simObj$ids$msm, simObj$ids$idu)
  # MSM_IDU = intersect(simObj$ids$msm, simObj$ids$idu)
  # nonMSM_noIDU = setdiff(simObj$ids$notmsm, simObj$ids$idu)
  #
  # IDU_notMSM_neg = setdiff(IDU_notMSM, simObj$popdf$id)
  # MSM_noIDU_neg = setdiff(MSM_noIDU, simObj$popdf$id)
  # MSM_IDU_neg = setdiff(MSM_IDU, simObj$popdf$id)
  # nonMSM_noIDU_neg = setdiff(nonMSM_noIDU, simObj$popdf$id)


  prep.df = tibble(id = NULL,
                   risk = NULL,
                   adherence = NULL)

  prep.df = bind_rows(prep.df,
                      prep_init_sample(risk_group = "idu",
                                       id_neg = simObj$negpopdf %>%
                                         filter(risk == "IDU") %>%
                                         pull(id),
                                       prep_info))

  prep.df = bind_rows(prep.df,
                      prep_init_sample(risk_group = "msm",
                                       id_neg = simObj$negpopdf %>%
                                         filter(risk == "MSM") %>%
                                         pull(id),
                                       prep_info))

  prep.df = bind_rows(prep.df,
                      prep_init_sample(risk_group = "msm",
                                       id_neg = simObj$negpopdf %>%
                                         filter(risk == "MSMandIDU") %>%
                                         pull(id),
                                       prep_info))

  prep.df = bind_rows(prep.df,
                      prep_init_sample(risk_group = "nonmsm",
                                       id_neg = simObj$negpopdf %>%
                                         filter(risk == "other") %>%
                                         pull(id),
                                       prep_info))

  simObj$prep_info = prep_info
  simObj$prep_id = prep.df

  return(simObj)
}


prep_init_sample <- function(risk_group, id_neg, prep_info) {

  size_sample = round(length(id_neg)*(prep_info %>%
                                        filter(param == "indication_per") %>%
                                        select(sym(risk_group)) %>%
                                        pull()))

  prep_neg = tibble(id = sample(id_neg,
                                size = size_sample,
                                replace = FALSE),
                    risk = risk_group,
                    reduction = sample(prep_info %>%
                                         filter(param %in% c("adherence_reduction_no", "adherence_reduction_low", "adherence_reduction_med", "adherence_reduction_high")) %>%
                                         select(sym(risk_group)) %>%
                                         pull(),
                                       size = size_sample,
                                       replace = TRUE,
                                       prob = prep_info %>%
                                         filter(param %in% c("adherence_per_no", "adherence_per_low", "adherence_per_med", "adherence_per_high")) %>%
                                         select(sym(risk_group)) %>%
                                         pull()),
                    use = sample(c(1,0),
                                 size = size_sample,
                                 replace = TRUE,
                                 prob = c(prep_info %>%
                                            filter(param %in% c("start_per")) %>%
                                            select(sym(risk_group)) %>%
                                            pull(),
                                          1- prep_info %>%
                                            filter(param %in% c("start_per")) %>%
                                            select(sym(risk_group)) %>%
                                            pull()))
  )

  return(prep_neg)

}

prep_update <- function(simObj) {

  #Discontinue PrEP
  prep_dis_list.df = left_join(simObj$prep_id %>% filter(use == 1),
                               simObj$prep_info %>%
                                 filter(param == "duration_mean") %>%
                                 select(-param) %>%
                                 pivot_longer(cols = c("msm", "idu", "nonmsm"),
                                              names_to = "risk",
                                              values_to = "duration_mean"),
                               by = "risk") %>%
    mutate(dis_prob = 1/duration_mean,
           dis_event = runif(nrow(.))<=dis_prob) %>%
    filter(dis_event == 1) %>%
    select(id) %>%
    mutate(update_use = 0)

  simObj$prep_id = left_join(simObj$prep_id, prep_dis_list.df, by = "id") %>%
    mutate(use = ifelse(!(is.na(update_use)),0, use)) %>%
    select(-update_use)

  #Start PrEP
  prep_start.df = left_join(simObj$prep_info %>%
                              filter(param == "growth_per") %>%
                              select(-param) %>%
                              pivot_longer(cols = c("msm", "idu", "nonmsm"),
                                           names_to = "risk",
                                           values_to = "growth_per"),
                            simObj$prep_info %>%
                              filter(param == "start_per") %>%
                              select(-param) %>%
                              pivot_longer(cols = c("msm", "idu", "nonmsm"),
                                           names_to = "risk",
                                           values_to = "start_per"),
                            by = "risk") %>%
    mutate(current_per = (growth_per/12) *simObj$month + start_per)

  prep_need.df = left_join(simObj$prep_id %>%
                             group_by(risk) %>%
                             summarise(n = n(),
                                       current_n = sum(use)),
                           prep_start.df,
                           by = "risk") %>%
    ungroup() %>%
    mutate(need = round(n*current_per - current_n)) %>%
    select(risk, need) %>%
    mutate(need = replace(need, which(need<0), 0))

  for (prep_group_id in c(1:nrow(prep_need.df))) {
    if (prep_need.df$need[prep_group_id] > 0){
      simObj$prep_id = left_join(simObj$prep_id,
                                 simObj$prep_id %>%
                                   filter(use == 0, risk == prep_need.df$risk[prep_group_id]) %>%
                                   slice_sample(n=prep_need.df$need[prep_group_id]) %>%
                                   mutate(update_use = 1) %>%
                                   select(id, update_use),
                                 by = "id") %>%
        mutate(use = ifelse(!(is.na(update_use)),1, use)) %>%
        select(-update_use)
    }
  }

  return(simObj)
}
