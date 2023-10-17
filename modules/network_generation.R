

generate_s_net <- function(simObj, init_net) {

  #simObj = preSimObj
  #init_net = TRUE

  if (init_net == TRUE) {

    demo_partner.df = tibble(
      risk = c("MSM", "IDU", "other", "MSMandIDU"),
      mean_partners = c(simObj$trans_params$Poisson_mean_MSM,
                        simObj$trans_params$Poisson_mean_nonMSM,
                        simObj$trans_params$Poisson_mean_nonMSM,
                        simObj$trans_params$Poisson_mean_MSM),
      MSMW_mean_partners = c(simObj$trans_params$Poisson_mean_nonMSM,
                             0,
                             0,
                             simObj$trans_params$Poisson_mean_nonMSM))

    simObj$popdf = left_join(simObj$popdf,
                             demo_partner.df,
                             by = join_by("risk" == "risk"),
                             relationship = "many-to-many")

    simObj$negpopdf = left_join(simObj$negpopdf,
                                demo_partner.df,
                                by = join_by("risk" == "risk"),
                                relationship = "many-to-many")

    simObj$popdf$MSMW = sample(as.integer(c(0,1)),
                             nrow(simObj$popdf),
                             prob =c(1-simObj$trans_params$MSMW_prob,
                                     simObj$trans_params$MSMW_prob),
                             replace = TRUE)
    simObj$popdf = simObj$popdf %>%
      mutate(MSMW = case_when(risk %in% c("MSM", "MSMandIDU") ~ MSMW,
                              .default = as.integer(0)))

    simObj$negpopdf$MSMW = sample(as.integer(c(0,1)),
                               nrow(simObj$negpopdf),
                               prob =c(1-simObj$trans_params$MSMW_prob,
                                       simObj$trans_params$MSMW_prob),
                               replace = TRUE)
    simObj$negpopdf = simObj$negpopdf %>%
      mutate(MSMW = case_when(risk %in% c("MSM", "MSMandIDU") ~ MSMW,
                              .default = as.integer(0)))
  }

  #################################
  ###Start of network generation###
  #################################

  #################################
  ###IDU degree sequence###
  #################################

  invalid_seq_IDU = TRUE

  while(invalid_seq_IDU) {

    if (init_net == TRUE) {

      simObj$popdf = simObj$popdf %>%
        select(-any_of(c("IDU_init_partners")))
      simObj$negpopdf = simObj$negpopdf %>%
        select(-any_of(c("IDU_init_partners")))

      popdf_TEMP = simObj$popdf %>%
        filter(risk %in% c("IDU", "MSMandIDU")) %>%
        mutate(mean_IDU_partners = simObj$trans_params$Poisson_mean_IDU) %>%
        select(id, mean_IDU_partners)

      popdf_TEMP$IDU_init_partners = as.integer(rpois(nrow(popdf_TEMP), lambda = popdf_TEMP$mean_IDU_partners))
      simObj$popdf = left_join(simObj$popdf,
                               popdf_TEMP %>% select(id, IDU_init_partners),
                               by = join_by(id))

      negpopdf_TEMP = simObj$negpopdf %>%
        filter(risk %in% c("IDU", "MSMandIDU")) %>%
        mutate(mean_IDU_partners = simObj$trans_params$Poisson_mean_IDU) %>%
        select(id, mean_IDU_partners)

      negpopdf_TEMP$IDU_init_partners = as.integer(rpois(nrow(negpopdf_TEMP), lambda = negpopdf_TEMP$mean_IDU_partners))
      simObj$negpopdf = left_join(simObj$negpopdf,
                                  negpopdf_TEMP %>% select(id, IDU_init_partners),
                                  by = join_by(id))

      deg_seq_IDU = c(
        simObj$popdf %>%
          filter(risk %in% c("IDU", "MSMandIDU")) %>%
          pull(IDU_init_partners),
        simObj$negpopdf  %>%
          filter(risk %in% c("IDU", "MSMandIDU")) %>%
          pull(IDU_init_partners)
      )
      if (sum(deg_seq_IDU) %% 2 == 0) {
        invalid_seq_IDU = FALSE
      }
    } else {
      #No evolution of IDU
      invalid_seq_IDU = FALSE
    }


  }

  #################################
  ###MSM degree sequence###
  #################################

  invalid_seq_MSM = TRUE

  while(invalid_seq_MSM) {

    if (init_net == TRUE) {

      simObj$popdf = simObj$popdf %>%
        select(-any_of(c("MSM_init_partners")))
      simObj$negpopdf = simObj$negpopdf %>%
        select(-any_of(c("MSM_init_partners")))

      popdf_TEMP = simObj$popdf %>%
        filter(risk %in% c("MSM", "MSMandIDU")) %>%
        select(id, mean_partners)

      popdf_TEMP$MSM_init_partners = as.integer(rpois(nrow(popdf_TEMP), lambda = popdf_TEMP$mean_partners))
      simObj$popdf = left_join(simObj$popdf,
                               popdf_TEMP %>% select(id, MSM_init_partners),
                               by = join_by(id))

      negpopdf_TEMP = simObj$negpopdf %>%
        filter(risk %in% c("MSM", "MSMandIDU")) %>%
        select(id, mean_partners)

      negpopdf_TEMP$MSM_init_partners = as.integer(rpois(nrow(negpopdf_TEMP), lambda = negpopdf_TEMP$mean_partners))
      simObj$negpopdf = left_join(simObj$negpopdf,
                               negpopdf_TEMP %>% select(id, MSM_init_partners),
                               by = join_by(id))

      deg_seq_MSM = c(
        simObj$popdf %>%
          filter(risk %in% c("MSM", "MSMandIDU")) %>%
          pull(MSM_init_partners),
        simObj$negpopdf  %>%
          filter(risk %in% c("MSM", "MSMandIDU")) %>%
          pull(MSM_init_partners)
      )

    } else {

      simObj$popdf = simObj$popdf %>%
        select(-any_of(c("MSM_evolve_partners")))
      simObj$negpopdf = simObj$negpopdf %>%
        select(-any_of(c("MSM_evolve_partners")))

      popdf_TEMP = simObj$popdf %>%
        filter(risk %in% c("MSM", "MSMandIDU")) %>%
        select(id, MSM_init_partners)

      popdf_TEMP$MSM_evolve_partners = as.integer(rpois(nrow(popdf_TEMP), lambda = popdf_TEMP$MSM_init_partners * (1/simObj$trans_params$Duration_MSM)))
      simObj$popdf = left_join(simObj$popdf,
                               popdf_TEMP %>% select(id, MSM_evolve_partners),
                               by = join_by(id))

      negpopdf_TEMP = simObj$negpopdf %>%
        filter(risk %in% c("MSM", "MSMandIDU")) %>%
        select(id, MSM_init_partners)

      negpopdf_TEMP$MSM_evolve_partners = as.integer(rpois(nrow(negpopdf_TEMP), lambda = negpopdf_TEMP$MSM_init_partners * (1/simObj$trans_params$Duration_MSM)))
      simObj$negpopdf = left_join(simObj$negpopdf,
                                  negpopdf_TEMP %>% select(id, MSM_evolve_partners),
                                  by = join_by(id))

      deg_seq_MSM = c(
        simObj$popdf %>%
          filter(risk %in% c("MSM", "MSMandIDU")) %>%
          pull(MSM_evolve_partners),
        simObj$negpopdf  %>%
          filter(risk %in% c("MSM", "MSMandIDU")) %>%
          pull(MSM_evolve_partners)
      )

    }

    if (sum(deg_seq_MSM) %% 2 == 0) {
      invalid_seq_MSM = FALSE
    }
  }

  #################################
  ###HET degree sequence###
  #################################

  invalid_seq_HET = TRUE

  while(invalid_seq_HET) {

    if (init_net == TRUE) {

      simObj$popdf = simObj$popdf %>%
        select(-any_of(c("HET_init_partners")))
      simObj$negpopdf = simObj$negpopdf %>%
        select(-any_of(c("HET_init_partners")))

      popdf_TEMP = simObj$popdf %>%
        filter(risk %in% c("other", "IDU")) %>%
        select(id, mean_partners)

      popdf_TEMP$HET_init_partners = as.integer(rpois(nrow(popdf_TEMP), lambda = popdf_TEMP$mean_partners))
      simObj$popdf = left_join(simObj$popdf,
                               popdf_TEMP %>% select(id, HET_init_partners),
                               by = join_by(id))

      negpopdf_TEMP = simObj$negpopdf %>%
        filter(risk %in% c("other", "IDU")) %>%
        select(id, mean_partners)

      negpopdf_TEMP$HET_init_partners = as.integer(rpois(nrow(negpopdf_TEMP), lambda = negpopdf_TEMP$mean_partners))
      simObj$negpopdf = left_join(simObj$negpopdf,
                                  negpopdf_TEMP %>% select(id, HET_init_partners),
                                  by = join_by(id))

      deg_seq_HET_F = c(
        simObj$popdf %>%
          filter(risk %in% c("other", "IDU")) %>%
          filter(gender == "female") %>%
          pull(HET_init_partners),
        simObj$negpopdf  %>%
          filter(risk %in% c("other", "IDU")) %>%
          filter(gender == "female") %>%
          pull(HET_init_partners)
      )

      deg_seq_HET_M = c(
        simObj$popdf %>%
          filter(risk %in% c("other", "IDU")) %>%
          filter(gender == "male") %>%
          pull(HET_init_partners),
        simObj$negpopdf  %>%
          filter(risk %in% c("other", "IDU")) %>%
          filter(gender == "male") %>%
          pull(HET_init_partners)
      )

    } else {

      simObj$popdf = simObj$popdf %>%
        select(-any_of(c("HET_evolve_partners")))
      simObj$negpopdf = simObj$negpopdf %>%
        select(-any_of(c("HET_evolve_partners")))

      popdf_TEMP = simObj$popdf %>%
        filter(risk %in% c("other", "IDU")) %>%
        select(id, HET_init_partners)

      popdf_TEMP$HET_evolve_partners = as.integer(rpois(nrow(popdf_TEMP), lambda = popdf_TEMP$HET_init_partners * (1/simObj$trans_params$Duration_nonMSM)))
      simObj$popdf = left_join(simObj$popdf,
                               popdf_TEMP %>% select(id, HET_evolve_partners),
                               by = join_by(id))

      negpopdf_TEMP = simObj$negpopdf %>%
        filter(risk %in% c("other", "IDU")) %>%
        select(id, HET_init_partners)

      negpopdf_TEMP$HET_evolve_partners = as.integer(rpois(nrow(negpopdf_TEMP), lambda = negpopdf_TEMP$HET_init_partners * (1/simObj$trans_params$Duration_nonMSM)))
      simObj$negpopdf = left_join(simObj$negpopdf,
                                  negpopdf_TEMP %>% select(id, HET_evolve_partners),
                                  by = join_by(id))

      deg_seq_HET_F = c(
        simObj$popdf %>%
          filter(risk %in% c("other", "IDU")) %>%
          filter(gender == "female") %>%
          pull(HET_evolve_partners),
        simObj$negpopdf  %>%
          filter(risk %in% c("other", "IDU")) %>%
          filter(gender == "female") %>%
          pull(HET_evolve_partners)
      )

      deg_seq_HET_M = c(
        simObj$popdf %>%
          filter(risk %in% c("other", "IDU")) %>%
          filter(gender == "male") %>%
          pull(HET_evolve_partners),
        simObj$negpopdf  %>%
          filter(risk %in% c("other", "IDU")) %>%
          filter(gender == "male") %>%
          pull(HET_evolve_partners)
      )

    }

    if (sum(deg_seq_HET_F) != sum(deg_seq_HET_M)) {
      adj_partners = simObj$trans_params$malefemale_partner_ratio_nonMSM
      adj_add_partners = round(abs(sum(deg_seq_HET_F) - sum(deg_seq_HET_M)) * adj_partners)
      adj_sub_partners = abs(sum(deg_seq_HET_F) - sum(deg_seq_HET_M)) - adj_add_partners

      simObj$popdf = simObj$popdf %>%
        select(-any_of(c("HET_partners_ADD")))
      simObj$negpopdf = simObj$negpopdf %>%
        select(-any_of(c("HET_partners_ADD")))
      simObj$popdf = simObj$popdf %>%
        select(-any_of(c("HET_partners_SUB")))
      simObj$negpopdf = simObj$negpopdf %>%
        select(-any_of(c("HET_partners_SUB")))

      if (sum(deg_seq_HET_F) > sum(deg_seq_HET_M)) {
        #add to males; sub from females
        Combpop.df = bind_rows(simObj$popdf, simObj$negpopdf)
        Combpop.df = left_join(Combpop.df,
                               Combpop.df %>%
                                 filter(gender == "male", risk %in% c("other", "IDU")) %>%
                                 slice_sample(n = adj_add_partners %>% sum(), replace = TRUE) %>%
                                 select(id) %>%
                                 group_by(id) %>%
                                 summarise(HET_partners_ADD = n()),
                               by = c("id" = "id")) %>%
          mutate(HET_partners_ADD = replace_na(HET_partners_ADD, as.integer(0)))

        if (init_net == TRUE) {
          Combpop.df = left_join(Combpop.df,
                                 Combpop.df %>%
                                   filter(gender == "female", risk %in% c("other", "IDU")) %>%
                                   select(id, HET_init_partners) %>%
                                   uncount(weight = HET_init_partners) %>%
                                   slice_sample(n = adj_sub_partners %>% sum(), replace = FALSE) %>%
                                   select(id) %>%
                                   group_by(id) %>%
                                   summarise(HET_partners_SUB = n()),
                                 by = c("id" = "id")) %>%
            mutate(HET_partners_SUB = replace_na(HET_partners_SUB, as.integer(0)))
        } else {
          Combpop.df = left_join(Combpop.df,
                                 Combpop.df %>%
                                   filter(gender == "female", risk %in% c("other", "IDU")) %>%
                                   select(id, HET_evolve_partners) %>%
                                   uncount(weight = HET_evolve_partners) %>%
                                   slice_sample(n = adj_sub_partners %>% sum(), replace = FALSE) %>%
                                   select(id) %>%
                                   group_by(id) %>%
                                   summarise(HET_partners_SUB = n()),
                                 by = c("id" = "id")) %>%
            mutate(HET_partners_SUB = replace_na(HET_partners_SUB, as.integer(0)))
        }
      } else {
        #add to females; sub from males
        Combpop.df = bind_rows(simObj$popdf, simObj$negpopdf)
        Combpop.df = left_join(Combpop.df,
                               Combpop.df %>%
                                 filter(gender == "female") %>%
                                 slice_sample(n = adj_add_partners %>% sum(), replace = TRUE) %>%
                                 select(id) %>%
                                 group_by(id) %>%
                                 summarise(HET_partners_ADD = n()),
                               by = c("id" = "id")) %>%
          mutate(HET_partners_ADD = replace_na(HET_partners_ADD, as.integer(0)))

        if (init_net == TRUE) {
          Combpop.df = left_join(Combpop.df,
                                 Combpop.df %>%
                                   filter(gender == "male", risk %in% c("other", "IDU")) %>%
                                   select(id, HET_init_partners) %>%
                                   uncount(weight = HET_init_partners) %>%
                                   slice_sample(n = adj_sub_partners %>% sum(), replace = FALSE) %>%
                                   select(id) %>%
                                   group_by(id) %>%
                                   summarise(HET_partners_SUB = n()),
                                 by = c("id" = "id")) %>%
            mutate(HET_partners_SUB = replace_na(HET_partners_SUB, as.integer(0)))
        } else {
          Combpop.df = left_join(Combpop.df,
                                 Combpop.df %>%
                                   filter(gender == "male", risk %in% c("other", "IDU")) %>%
                                   select(id, HET_evolve_partners) %>%
                                   uncount(weight = HET_evolve_partners) %>%
                                   slice_sample(n = adj_sub_partners %>% sum(), replace = FALSE) %>%
                                   select(id) %>%
                                   group_by(id) %>%
                                   summarise(HET_partners_SUB = n()),
                                 by = c("id" = "id")) %>%
            mutate(HET_partners_SUB = replace_na(HET_partners_SUB, as.integer(0)))
        }
      }
      if (init_net == TRUE) {
        simObj$popdf = left_join(simObj$popdf,
                                 Combpop.df %>% select(id, HET_partners_ADD, HET_partners_SUB),
                                 by = c("id" = "id")) %>%
        mutate(HET_init_partners = HET_init_partners + HET_partners_ADD) %>%
        mutate(HET_init_partners = HET_init_partners - HET_partners_SUB)

        simObj$negpopdf = left_join(simObj$negpopdf,
                                    Combpop.df %>% select(id, HET_partners_ADD, HET_partners_SUB),
                                    by = c("id" = "id")) %>%
          mutate(HET_init_partners = HET_init_partners + HET_partners_ADD) %>%
          mutate(HET_init_partners = HET_init_partners - HET_partners_SUB)

        deg_seq_HET_F = c(
          simObj$popdf %>%
            filter(risk %in% c("other", "IDU")) %>%
            filter(gender == "female") %>%
            pull(HET_init_partners),
          simObj$negpopdf  %>%
            filter(risk %in% c("other", "IDU")) %>%
            filter(gender == "female") %>%
            pull(HET_init_partners)
        )

        deg_seq_HET_M = c(
          simObj$popdf %>%
            filter(risk %in% c("other", "IDU")) %>%
            filter(gender == "male") %>%
            pull(HET_init_partners),
          simObj$negpopdf  %>%
            filter(risk %in% c("other", "IDU")) %>%
            filter(gender == "male") %>%
            pull(HET_init_partners)
        )

      } else {
        simObj$popdf = left_join(simObj$popdf,
                                 Combpop.df %>% select(id, HET_partners_ADD, HET_partners_SUB),
                                 by = c("id" = "id")) %>%
          mutate(HET_evolve_partners = HET_evolve_partners + HET_partners_ADD,
                 HET_evolve_partners = HET_evolve_partners - HET_partners_SUB)

        simObj$negpopdf = left_join(simObj$negpopdf,
                                    Combpop.df %>% select(id, HET_partners_ADD, HET_partners_SUB),
                                    by = c("id" = "id")) %>%
          mutate(HET_evolve_partners = HET_evolve_partners + HET_partners_ADD,
                 HET_evolve_partners = HET_evolve_partners - HET_partners_SUB)

        deg_seq_HET_F = c(
          simObj$popdf %>%
            filter(risk %in% c("other", "IDU")) %>%
            filter(gender == "female") %>%
            pull(HET_evolve_partners),
          simObj$negpopdf  %>%
            filter(risk %in% c("other", "IDU")) %>%
            filter(gender == "female") %>%
            pull(HET_evolve_partners)
        )

        deg_seq_HET_M = c(
          simObj$popdf %>%
            filter(risk %in% c("other", "IDU")) %>%
            filter(gender == "male") %>%
            pull(HET_evolve_partners),
          simObj$negpopdf  %>%
            filter(risk %in% c("other", "IDU")) %>%
            filter(gender == "male") %>%
            pull(HET_evolve_partners)
        )
      }
    }

    if (sum(deg_seq_HET_F) == sum(deg_seq_HET_M)) {
      #adjust
      invalid_seq_HET = FALSE
    }
  }

  #################################
  ###MSMW degree sequence###
  #################################

  if (init_net == TRUE) {

    popdf_TEMP = simObj$popdf %>%
      filter(MSMW == 1) %>%
      select(id, MSMW_mean_partners)

    popdf_TEMP$MSMW_init_partners = as.integer(rpois(nrow(popdf_TEMP), lambda = popdf_TEMP$MSMW_mean_partners))
    simObj$popdf = left_join(simObj$popdf,
                             popdf_TEMP %>% select(id, MSMW_init_partners),
                             by = join_by(id)) %>%
      mutate(MSMW_init_partners = replace_na(MSMW_init_partners, as.integer(0)))

    negpopdf_TEMP = simObj$negpopdf %>%
      filter(MSMW == 1) %>%
      select(id, MSMW_mean_partners)

    negpopdf_TEMP$MSMW_init_partners = as.integer(rpois(nrow(negpopdf_TEMP), lambda = negpopdf_TEMP$MSMW_mean_partners))
    simObj$negpopdf = left_join(simObj$negpopdf,
                                negpopdf_TEMP %>% select(id, MSMW_init_partners),
                                by = join_by(id)) %>%
      mutate(MSMW_init_partners = replace_na(MSMW_init_partners, as.integer(0)))

  } else {

    simObj$popdf = simObj$popdf %>%
      select(-any_of(c("MSMW_evolve_partners")))
    simObj$negpopdf = simObj$negpopdf %>%
      select(-any_of(c("MSMW_evolve_partners")))

    popdf_TEMP = simObj$popdf %>%
      filter(MSMW == 1) %>%
      select(id, MSMW_init_partners)

    popdf_TEMP$MSMW_evolve_partners = as.integer(rpois(nrow(popdf_TEMP), lambda = popdf_TEMP$MSMW_init_partners * (1/simObj$trans_params$Duration_nonMSM)))
    simObj$popdf = left_join(simObj$popdf,
                             popdf_TEMP %>% select(id, MSMW_evolve_partners),
                             by = join_by(id)) %>%
      mutate(MSMW_evolve_partners = replace_na(MSMW_evolve_partners, as.integer(0)))

    negpopdf_TEMP = simObj$negpopdf %>%
      filter(MSMW == 1) %>%
      select(id, MSMW_init_partners)

    negpopdf_TEMP$MSMW_evolve_partners = as.integer(rpois(nrow(negpopdf_TEMP), lambda = negpopdf_TEMP$MSMW_init_partners * (1/simObj$trans_params$Duration_nonMSM)))
    simObj$negpopdf = left_join(simObj$negpopdf,
                                negpopdf_TEMP %>% select(id, MSMW_evolve_partners),
                                by = join_by(id)) %>%
      mutate(MSMW_evolve_partners = replace_na(MSMW_evolve_partners, as.integer(0)))

  }

  Combpop.df = bind_rows(simObj$popdf, simObj$negpopdf)

  if (init_net == TRUE) {
  Combpop.df = left_join(Combpop.df,
                         Combpop.df %>%
                             filter(gender == "female") %>%
                             slice_sample(n = Combpop.df$MSMW_init_partners %>% sum(), replace = TRUE) %>%
                             select(id) %>%
                             group_by(id) %>%
                             summarise(MSMW_F_init_partners = n()),
                           by = c("id" = "id")) %>%
      mutate(MSMW_F_init_partners = replace_na(MSMW_F_init_partners, as.integer(0)))
  } else {
    Combpop.df = left_join(Combpop.df,
                           Combpop.df %>%
                             filter(gender == "female") %>%
                             slice_sample(n = Combpop.df$MSMW_evolve_partners %>% sum(), replace = TRUE) %>%
                             select(id) %>%
                             group_by(id) %>%
                             summarise(MSMW_F_evolve_partners = n()),
                           by = c("id" = "id")) %>%
      mutate(MSMW_F_evolve_partners = replace_na(MSMW_F_evolve_partners, as.integer(0)))  }


  #################################
  ### network generation ###
  #################################

  Combpop.df = Combpop.df %>%
    mutate(hiv_status = as.numeric(!is.na(infectdur)))

  #################################
  ###MSM network generation###
  #################################

  MSM_net.edgelist = generate_s_net_R(network_type = "MSM",
                                      init_net = init_net,
                                      simObj = simObj,
                                      Combpop.df = Combpop.df
  )

##########
  # MSM_net.edgelist_check = left_join(MSM_net.edgelist, Combpop.df %>% select(id, risk, hiv_status),
  #                          by = join_by(source == id)) %>%
  #   rename(risk_source = risk,
  #          hiv_status_source = hiv_status) %>%
  #   left_join(Combpop.df %>% select(id, risk, hiv_status),
  #             by = join_by(target == id)) %>%
  #   rename(risk_target = risk,
  #          hiv_status_target = hiv_status)
  #
  # sum(MSM_net.edgelist_check$risk_source %in% c("MSM", "MSMandIDU")) == nrow(MSM_net.edgelist_check)
  # sum(MSM_net.edgelist_check$risk_target %in% c("MSM", "MSMandIDU")) == nrow(MSM_net.edgelist_check)
#########

  # if (init_net == TRUE) {
  #   deg_seq_MSM = Combpop.df %>%
  #     filter(risk %in% c("MSM", "MSMandIDU"), MSM_init_partners > 0) %>%
  #     mutate(networkx_id = c(0:(n()-1))) %>%
  #     mutate(MSM_partners = MSM_init_partners)
  #   num_assort_factors = 2
  # } else {
  #   deg_seq_MSM = Combpop.df %>%
  #     filter(risk %in% c("MSM", "MSMandIDU"), MSM_evolve_partners > 0) %>%
  #     mutate(networkx_id = c(0:(n()-1))) %>%
  #     mutate(MSM_partners = MSM_evolve_partners)
  #   num_assort_factors = 1
  # }

  # MSM_net.edgelist = generate_s_net_py(
  #   network_type = "MSM",
  #   deg_seq_1 = deg_seq_MSM$MSM_partners,
  #   deg_seq_2 = NULL,
  #   num_assort_factors = num_assort_factors,
  #   node_ids = deg_seq_MSM$networkx_id,
  #   factor_1_list = deg_seq_MSM$risk,
  #   factor_1_assort = 0.2,
  #   factor_2_list = deg_seq_MSM$hiv_status,
  #   factor_2_assort = 0.2) %>%
  #   as_tibble()
  #
  # MSM_net.edgelist = left_join(MSM_net.edgelist, deg_seq_MSM %>% select(id, networkx_id),
  #           by = join_by(source == networkx_id)) %>%
  #   select(-source) %>%
  #   rename(source = id) %>%
  #   left_join(deg_seq_MSM %>% select(id, networkx_id),
  #             by = join_by(target == networkx_id)) %>%
  #   select(-target) %>%
  #   rename(target = id)

  ##Check
  #sum(MSM_net.edgelist$source %in% deg_seq_MSM$id) == nrow(MSM_net.edgelist)
  #sum(MSM_net.edgelist$target %in% deg_seq_MSM$id) == nrow(MSM_net.edgelist)
  ##

  #################################
  ###HET network generation###
  #################################

  HET_net.edgelist = generate_s_net_R(network_type = "HET",
                                      init_net = init_net,
                                      simObj = simObj,
                                      Combpop.df = Combpop.df
  )

  # if (init_net == TRUE) {
  #   deg_seq_HET_F = Combpop.df %>%
  #     filter(risk %in% c("other", "IDU")) %>%
  #     filter(gender == "female") %>%
  #     mutate(networkx_id = c(0:(n()-1))) %>%
  #     mutate(HET_partners = HET_init_partners)
  #
  #   deg_seq_HET_M = Combpop.df %>%
  #     filter(risk %in% c("other", "IDU")) %>%
  #     filter(gender == "male") %>%
  #     mutate(networkx_id = c(nrow(deg_seq_HET_F):(nrow(deg_seq_HET_F) + n()-1))) %>%
  #     mutate(HET_partners = HET_init_partners)
  #
  #   num_assort_factors = 2
  # } else {
  #   deg_seq_HET_F = Combpop.df %>%
  #     filter(risk %in% c("other", "IDU")) %>%
  #     filter(gender == "female") %>%
  #     mutate(networkx_id = c(0:(n()-1))) %>%
  #     mutate(HET_partners = HET_evolve_partners)
  #     pull(HET_evolve_partners)
  #
  #   deg_seq_HET_M = Combpop.df %>%
  #     filter(risk %in% c("other", "IDU")) %>%
  #     filter(gender == "male") %>%
  #     mutate(networkx_id = c(nrow(deg_seq_HET_F):(nrow(deg_seq_HET_F) + n()-1))) %>%
  #     mutate(HET_partners = HET_evolve_partners)
  #     pull(HET_evolve_partners)
  #
  #   num_assort_factors = 1
  # }
  #
  # HET_net.edgelist = generate_s_net_py(
  #   network_type = "HET",
  #   deg_seq_1 = deg_seq_HET_F$HET_partners,
  #   deg_seq_2 = deg_seq_HET_M$HET_partners,
  #   num_assort_factors = num_assort_factors,
  #   factor_1_list = NULL,
  #   factor_1_assort = 0,
  #   factor_2_list = NULL,
  #   factor_2_assort = 0)
  #
  # HET_net.edgelist = left_join(HET_net.edgelist, deg_seq_HET_F %>% select(id, networkx_id),
  #                              by = join_by(source == networkx_id)) %>%
  #   select(-source) %>%
  #   rename(source = id) %>%
  #   left_join(deg_seq_HET_M %>% select(id, networkx_id),
  #             by = join_by(target == networkx_id)) %>%
  #   select(-target) %>%
  #   rename(target = id)

  ##########
  # HET_net.edgelist_check = left_join(HET_net.edgelist, Combpop.df %>% select(id, risk, hiv_status),
  #                                    by = join_by(source == id)) %>%
  #   rename(risk_source = risk,
  #          hiv_status_source = hiv_status) %>%
  #   left_join(Combpop.df %>% select(id, risk, hiv_status),
  #             by = join_by(target == id)) %>%
  #   rename(risk_target = risk,
  #          hiv_status_target = hiv_status)
  #
  # sum(HET_net.edgelist_check$risk_source %in% c("other", "IDU")) == nrow(HET_net.edgelist_check)
  # sum(HET_net.edgelist_check$risk_target %in% c("other", "IDU")) == nrow(HET_net.edgelist_check)
  #########

  #################################
  ###MSMW network generation###
  #################################

  MSMW_net.edgelist = generate_s_net_R(network_type = "MSMW",
                                      init_net = init_net,
                                      simObj = simObj,
                                      Combpop.df = Combpop.df
  )

  # if (init_net == TRUE) {
  #   deg_seq_MSMW = Combpop.df %>%
  #     filter(risk %in% c("MSM", "MSMandIDU")) %>%
  #     mutate(networkx_id = c(0:(n()-1))) %>%
  #     mutate(MSMW_partners = MSMW_init_partners)
  #
  #   deg_seq_MSMW_F = Combpop.df %>%
  #     filter(gender == "female") %>%
  #     mutate(networkx_id = c(nrow(deg_seq_MSMW):(nrow(deg_seq_MSMW) + n()-1))) %>%
  #     mutate(MSMW_partners = MSMW_F_init_partners)
  #
  #   num_assort_factors = 2
  # } else {
  #   deg_seq_MSMW = Combpop.df %>%
  #     filter(risk %in% c("MSM", "MSMandIDU")) %>%
  #     mutate(networkx_id = c(0:(n()-1))) %>%
  #     mutate(MSMW_partners = MSMW_evolve_partners)
  #
  #   deg_seq_MSMW_F = Combpop.df %>%
  #     filter(gender == "female") %>%
  #     mutate(networkx_id = c(nrow(deg_seq_MSMW):(nrow(deg_seq_MSMW) + n()-1))) %>%
  #     mutate(MSMW_partners = MSMW_F_evolve_partners)
  #
  #   num_assort_factors = 1
  # }
  #
  # MSMW_net.edgelist = generate_s_net_py(
  #   network_type = "MSMW",
  #   deg_seq_1 = deg_seq_MSMW$MSMW_partners,
  #   deg_seq_2 = deg_seq_MSMW_F$MSMW_partners,
  #   num_assort_factors = 0,
  #   factor_1_list = NULL,
  #   factor_1_assort = NULL,
  #   factor_2_list = NULL,
  #   factor_2_assort = NULL)
  #
  # MSMW_net.edgelist = left_join(MSMW_net.edgelist, deg_seq_MSMW %>% select(id, networkx_id),
  #                              by = join_by(source == networkx_id)) %>%
  #   select(-source) %>%
  #   rename(source = id) %>%
  #   left_join(deg_seq_MSMW_F %>% select(id, networkx_id),
  #             by = join_by(target == networkx_id)) %>%
  #   select(-target) %>%
  #   rename(target = id)


  if (init_net == TRUE) {
    IDU_net.edgelist = generate_s_net_R(network_type = "IDU",
                                        init_net = init_net,
                                        simObj = simObj,
                                        Combpop.df = Combpop.df
    )
  }

  #################################
  ###Dissolve network###
  #################################

  if (init_net == FALSE) {
    for (Net_name in c("MSM", "HET")) {
      if (Net_name == "MSM") {
        partner_duration = simObj$trans_params$Duration_MSM
      } else {
        partner_duration = simObj$trans_params$Duration_nonMSM
      }
      simObj$networks[[Net_name]] <- simObj$networks[[Net_name]] %>%
        mutate(trans_event = runif(nrow(.))<=1/partner_duration)

      simObj$networks[[Net_name]] <-
        simObj$networks[[Net_name]][-which(simObj$networks[[Net_name]]$trans_event == 1),]
    }
  }

  #################################
  ###Add network###
  #################################

  if (init_net == TRUE) {

    HET_net.edgelist = HET_net.edgelist %>%
      rename(ID1 = source,
             ID2 = target)

    MSM_net.edgelist = MSM_net.edgelist %>%
      rename(ID1 = source,
             ID2 = target)

    MSMW_net.edgelist = MSMW_net.edgelist %>%
      rename(ID1 = source,
             ID2 = target)

    IDU_net.edgelist = IDU_net.edgelist %>%
      rename(ID1 = source,
             ID2 = target)

    simObj$networks$HET <- bind_rows(HET_net.edgelist,
                                     MSMW_net.edgelist)

    simObj$networks$MSM <- MSM_net.edgelist
    simObj$networks$IDU <- IDU_net.edgelist

  } else {

    HET_net.edgelist = HET_net.edgelist %>%
      rename(ID1 = source,
             ID2 = target)

    MSM_net.edgelist = MSM_net.edgelist %>%
      rename(ID1 = source,
             ID2 = target)

    MSMW_net.edgelist = MSMW_net.edgelist %>%
      rename(ID1 = source,
             ID2 = target)

    simObj$networks$HET <- bind_rows(simObj$networks$HET,
                                     bind_rows(HET_net.edgelist,
                                               MSMW_net.edgelist))

    simObj$networks$MSM <- bind_rows(simObj$networks$MSM,
                                     MSM_net.edgelist)
  }

  #################################
  ###Store network###
  #################################

  simObj$networks$S_MSM_Net = generate_s_network(simObj, Combpop.df, Net_name = "MSM")
  simObj$networks$S_nonMSM_Net = generate_s_network(simObj, Combpop.df, Net_name = "HET")
  simObj$networks$IDU_Net = generate_s_network(simObj, Combpop.df, Net_name = "IDU")

  return(simObj)
}

generate_s_network <- function(simObj, totpopdf, Net_name) {

  simObj$networks[[Net_name]] = simObj$networks[[Net_name]] %>%
    rename(source = ID1,
          target = ID2)

  g_edge_list = left_join(simObj$networks[[Net_name]],
                          totpopdf %>% select(id, risk, stage) ,
                          by = c("source" = "id")) %>%
    rename(risk1 = risk,
           stage1 = stage) %>%
    left_join(totpopdf %>% select(id, risk, stage) ,
              by = c("target" = "id"))%>%
    rename(risk2 = risk,
           stage2 = stage) %>%
    filter(stage1 != "at_risk" | stage2 != "at_risk") %>%
    filter(!(stage1 != "at_risk" &  stage2 != "at_risk"))

  #HIV as ID1 and at_risk as ID2
  g_edge_list = g_edge_list %>%
    mutate(ID1 = case_when(stage1 == "at_risk" ~ target,
                           .default = source)) %>%
    mutate(ID2 = case_when(stage1 == "at_risk" ~ source,
                           .default = target)) %>%
    select(ID1, ID2)

  simObj$networks[[Net_name]] = simObj$networks[[Net_name]] %>%
    rename(ID1 = source,
           ID2 = target)

  return(g_edge_list)
}




  # if (init_net) {
  #
  #   generate_s_net_py()
  #
  #   s_contact.net = sample_degseq(out.deg = popdf_TEMP$init_partners,
  #                                 in.deg = NULL,
  #                                 method = "simple")
  #
  # } else {
  #   popdf_TEMP$evolve_partners = rpois(nrow(popdf_TEMP), lambda = popdf_TEMP$init_partners * popdf_TEMP$evolve_partners)
  #   s_contact.net = sample_degseq(out.deg = popdf_TEMP$evolve_partners,
  #                                 in.deg = NULL,
  #                                 method = "simple")
  # }
  #
  # s_contact.assort = c()
  # for (characteristic in network_assort$characteristic) {
  #   s_contact.assort = c(s_contact.assort,
  #                        assortativity_nominal(s_contact.net,
  #                                              types = popdf_TEMP[[characteristic]] %>%
  #                                                as.factor() %>%
  #                                                as.numeric(),
  #                                              directed = FALSE))
  #
  # }
  #
  # for (i in c(1:10)) {
  #
  #   s_contact_TEMP.net = s_contact.net %>%
  #     rewire(keeping_degseq(niter = 20))
  #
  #   s_contact_TEMP.assort = c()
  #   for (characteristic in network_assort$characteristic) {
  #     s_contact_TEMP.assort = c(s_contact_TEMP.assort,
  #                          assortativity_nominal(s_contact_TEMP.net,
  #                                                types = popdf_TEMP[[characteristic]] %>%
  #                                                  as.factor() %>%
  #                                                  as.numeric(),
  #                                                directed = FALSE))
  #
  #   }
  #
  #   if (sum(abs(s_contact_TEMP.assort - network_assort$assort_value)) < sum(abs(s_contact.assort - network_assort$assort_value)) ) {
  #     s_contact.net = s_contact_TEMP.net
  #     s_contact.assort = s_contact_TEMP.assort
  #   }
  # }
  #
  # return(s_contact.net)

# gen_s_net_split <- function(preSimObj) {
#
#   g_edge_list = as_tibble(as_edgelist(graph = preSimObj$network_s_complete))
#   colnames(g_edge_list) = c("ID1", "ID2")
#
#   totpopdf = bind_rows(simObj$popdf, simObj$negpopdf)
#
#   g_edge_list = left_join(g_edge_list,
#                           totpopdf %>% select(id, risk, stage) ,
#                           by = c("ID1_temp" = "id")) %>%
#     rename(risk1 = risk,
#            stage1 = stage) %>%
#     left_join(totpopdf %>% select(id, risk, stage) ,
#               by = c("ID2_temp" = "id"))%>%
#     rename(risk2 = risk,
#            stage2 = stage) %>%
#     filter(stage1 != "at_risk" | stage2 != "at_risk") %>%
#     filter(!(stage1 != "at_risk" &  stage2 != "at_risk"))
#
#   #HIV as ID1 and at_risk as ID2
#   g_edge_list = g_edge_list %>%
#     mutate(ID1 = case_when(stage1 == "at_risk" ~ ID2_temp,
#                                  .default = ID1_temp)) %>%
#     mutate(ID2 = case_when(stage1 == "at_risk" ~ ID1_temp,
#                                  .default = ID2_temp))
#   #Split into MSM and non-MSM
#   g_edge_list_MSM =  g_edge_list %>%
#     filter(risk1 %in% c("MSM", "MSMandIDU") & risk2 %in% c("MSM", "MSMandIDU")) %>%
#     select()
#
#   g_edge_list_nonMSM =  g_edge_list %>%
#     filter(!(risk1 %in% c("MSM", "MSMandIDU") & risk2 %in% c("MSM", "MSMandIDU")))
# }





