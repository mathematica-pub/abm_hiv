

generate_s_net_R <- function(network_type,
                             init_net,
                             simObj,
                             Combpop.df
                             ) {

  if (network_type == "MSM") {
    if (init_net == TRUE) {

      deg_seq_net = Combpop.df %>%
        filter(risk %in% c("MSM", "MSMandIDU"), MSM_init_partners > 0) %>%
        mutate(MSM_partners = MSM_init_partners)

      factor_1_assort = simObj$trans_params$Assortativity_MSM %>% filter(`Age Category` == "risk") %>% pull(Value)
      factor_2_assort = simObj$trans_params$Assortativity_MSM %>% filter(`Age Category` == "hiv_status") %>% pull(Value)

      deg_seq_net = deg_seq_net %>%
        mutate(block = case_when((risk == "MSM" & hiv_status == "0") ~ 1,
                                 (risk == "MSM" & hiv_status == "1") ~ 2,
                                 (risk == "MSMandIDU" & hiv_status == "0") ~ 3,
                                 (risk == "MSMandIDU" & hiv_status == "1") ~ 4)) %>%
        arrange(block)
    } else {

      deg_seq_net = Combpop.df %>%
        filter(risk %in% c("MSM", "MSMandIDU"), MSM_evolve_partners > 0) %>%
        mutate(MSM_partners = MSM_evolve_partners)

      factor_1_assort = simObj$trans_params$Assortativity_MSM %>% filter(`Age Category` == "risk") %>% pull(Value)
      factor_2_assort = NULL

      deg_seq_net = deg_seq_net %>%
        mutate(block = case_when((risk == "MSM") ~ 1,
                                 (risk == "MSMandIDU") ~ 2)) %>%
        arrange(block)
    }
    deg_seq_net = deg_seq_net %>%
      mutate(networkx_id = c(1:(n())))

    dcsbm_theta = deg_seq_net$MSM_partners
  } else if (network_type == "HET") {

    if (init_net == TRUE) {

      deg_seq_net = Combpop.df %>%
        filter(risk %in% c("other", "IDU"), HET_init_partners > 0) %>%
        mutate(HET_partners = HET_init_partners)

      factor_1_assort = simObj$trans_params$Assortativity_nonMSM %>% filter(`Age Category` == "Risk") %>% pull(Value)
      factor_2_assort = simObj$trans_params$Assortativity_nonMSM %>% filter(`Age Category` == "HIV_status") %>% pull(Value)

      deg_seq_net = deg_seq_net %>%
        mutate(block = case_when((gender == "female" & risk == "other" & hiv_status == "0") ~ 1,
                                 (gender == "female" & risk == "other" & hiv_status == "1") ~ 2,
                                 (gender == "female" & risk == "IDU" & hiv_status == "0") ~ 3,
                                 (gender == "female" & risk == "IDU" & hiv_status == "1") ~ 4,
                                 (gender == "male" & risk == "other" & hiv_status == "0") ~ 5,
                                 (gender == "male" & risk == "other" & hiv_status == "1") ~ 6,
                                 (gender == "male" & risk == "IDU" & hiv_status == "0") ~ 7,
                                 (gender == "male" & risk == "IDU" & hiv_status == "1") ~ 8
                                 )) %>%
        arrange(block)
    } else {

      deg_seq_net = Combpop.df %>%
        filter(risk %in% c("other", "IDU"), HET_evolve_partners > 0) %>%
        mutate(HET_partners = HET_evolve_partners)

      factor_1_assort = simObj$trans_params$Assortativity_nonMSM %>% filter(`Age Category` == "Risk") %>% pull(Value)
      factor_2_assort = NULL

      deg_seq_net = deg_seq_net %>%
        mutate(block = case_when((gender == "female" & risk == "other") ~ 1,
                                 (gender == "female" & risk == "IDU") ~ 2,
                                 (gender == "male" & risk == "other") ~ 3,
                                 (gender == "male" & risk == "IDU") ~ 4
        )) %>%
        arrange(block)
    }
    deg_seq_net = deg_seq_net %>%
      mutate(networkx_id = c(1:(n())))

    dcsbm_theta = deg_seq_net$HET_partners

  } else if (network_type == "MSMW") {
    if (init_net == TRUE) {

      deg_seq_net = Combpop.df %>%
        mutate(MSMW_partners = MSMW_init_partners + MSMW_F_init_partners) %>%
        filter(MSMW_partners > 0)


      factor_1_assort = simObj$trans_params$Assortativity_nonMSM %>% filter(`Age Category` == "Risk") %>% pull(Value)
      factor_2_assort = simObj$trans_params$Assortativity_nonMSM %>% filter(`Age Category` == "HIV_status") %>% pull(Value)

      deg_seq_net = deg_seq_net %>%
        mutate(block = case_when((gender == "female" & risk == "other" & hiv_status == "0") ~ 1,
                                 (gender == "female" & risk == "other" & hiv_status == "1") ~ 2,
                                 (gender == "female" & risk == "IDU" & hiv_status == "0") ~ 3,
                                 (gender == "female" & risk == "IDU" & hiv_status == "1") ~ 4,
                                 (gender == "male" & risk == "MSM" & hiv_status == "0") ~ 5,
                                 (gender == "male" & risk == "MSM" & hiv_status == "1") ~ 6,
                                 (gender == "male" & risk == "MSMandIDU" & hiv_status == "0") ~ 7,
                                 (gender == "male" & risk == "MSMandIDU" & hiv_status == "1") ~ 8
        )) %>%
        arrange(block)
    } else {

      deg_seq_net = Combpop.df %>%
        mutate(MSMW_partners = MSMW_evolve_partners + MSMW_F_evolve_partners) %>%
        filter(MSMW_partners > 0)

      factor_1_assort = simObj$trans_params$Assortativity_nonMSM %>% filter(`Age Category` == "Risk") %>% pull(Value)
      factor_2_assort = NULL

      deg_seq_net = deg_seq_net %>%
        mutate(block = case_when((gender == "female" & risk == "other") ~ 1,
                                 (gender == "female" & risk == "IDU") ~ 2,
                                 (gender == "male" & risk == "MSM") ~ 3,
                                 (gender == "male" & risk == "MSMandIDU") ~ 4
        )) %>%
        arrange(block)
    }
    deg_seq_net = deg_seq_net %>%
      mutate(networkx_id = c(1:(n())))

    dcsbm_theta = deg_seq_net$MSMW_partners

  } else if (network_type == "IDU") {
    if (init_net == TRUE) {

      deg_seq_net = Combpop.df %>%
        filter(risk %in% c("IDU", "MSMandIDU"), IDU_init_partners > 0) %>%
        mutate(IDU_partners = IDU_init_partners)

      factor_1_assort = simObj$trans_params$discordant_IDU

      deg_seq_net = deg_seq_net %>%
        mutate(block = case_when((hiv_status == "0") ~ 1,
                                 (hiv_status == "1") ~ 2)) %>%
        arrange(block)
    } else {
      print("ERROR")
    }
    deg_seq_net = deg_seq_net %>%
      mutate(networkx_id = c(1:(n())))

    dcsbm_theta = deg_seq_net$IDU_partners

  } else {
    print("ERROR")
  }

  dcsbm_B = generate_dcsbm_b_matrix(network_type,
                                    init_net,
                                    factor_1_assort,
                                    factor_2_assort) %>%
    as.matrix()

  dcsbm_pi = as.numeric(tabulate(deg_seq_net$block, nbins = (dcsbm_B %>% nrow())))/nrow(deg_seq_net)

  g = dcsbm(
    theta = as.numeric(dcsbm_theta),
    B = dcsbm_B,
    expected_density = (sum(dcsbm_theta)/2)/choose(length(dcsbm_theta),2),
    pi = dcsbm_pi,
    sort_nodes = FALSE,
    poisson_edges = FALSE,
    allow_self_loops = FALSE
  )

  edgelist <- sample_edgelist(g)

  net.edgelist = left_join(edgelist, deg_seq_net %>% select(id, networkx_id),
                               by = join_by(from == networkx_id)) %>%
    select(-from) %>%
    rename(source = id) %>%
    left_join(deg_seq_net %>% select(id, networkx_id),
              by = join_by(to == networkx_id)) %>%
    select(-to) %>%
    rename(target = id)

   # Temp = left_join(edgelist, deg_seq_net %>% select(id, networkx_id),
   #                         by = join_by(from == networkx_id)) %>%
   #  rename(source = id) %>%
   #  left_join(deg_seq_net %>% select(id, networkx_id),
   #            by = join_by(to == networkx_id)) %>%
   #  rename(target = id)

  return(net.edgelist)

}

generate_dcsbm_b_matrix <- function(network_type,
                                  init_net,
                                  factor_1_assort,
                                  factor_2_assort) {

  if (network_type %in% c("MSM", "HET", "MSMW")) {
    if (init_net == TRUE) {

      dcsbm_B = matrix(data = c(factor_1_assort*factor_2_assort,
                                factor_1_assort*(1-factor_2_assort),
                                (1-factor_1_assort)*factor_2_assort,
                                (1-factor_1_assort)*(1-factor_2_assort),

                                factor_1_assort*(1-factor_2_assort),
                                factor_1_assort*(factor_2_assort),
                                (1-factor_1_assort)*(1-factor_2_assort),
                                (1-factor_1_assort)*(factor_2_assort),

                                (1-factor_1_assort)*factor_2_assort,
                                (1-factor_1_assort)*(1-factor_2_assort),
                                factor_1_assort*factor_2_assort,
                                factor_1_assort*(1-factor_2_assort),

                                (1-factor_1_assort)*(1-factor_2_assort),
                                (1-factor_1_assort)*(factor_2_assort),
                                factor_1_assort*(1-factor_2_assort),
                                (factor_1_assort)*(factor_2_assort)
      ),
      nrow = 4,
      ncol = 4)
    } else {
      dcsbm_B = matrix(data = c(factor_1_assort,
                                (1-factor_1_assort),
                                (1-factor_1_assort),
                                factor_1_assort

      ),
      nrow = 2,
      ncol = 2)
    }
  }
  if (network_type %in% c("HET", "MSMW")) {
    dcsbm_B_zero = matrix(data = 0,
                          nrow = dim(dcsbm_B)[1],
                          ncol = dim(dcsbm_B)[2])
    dcsbm_B_1 = suppressMessages(bind_cols(dcsbm_B_zero, dcsbm_B))
    dcsbm_B_2 = suppressMessages(bind_cols(dcsbm_B, dcsbm_B_zero))
    dcsbm_B = suppressMessages(bind_rows(dcsbm_B_1, dcsbm_B_2))

  }

  if (network_type %in% c("IDU")) {
    if (init_net == TRUE) {
    dcsbm_B = matrix(data = c(factor_1_assort,
                              (1-factor_1_assort),
                              (1-factor_1_assort),
                              factor_1_assort

    ),
    nrow = 2,
    ncol = 2)
    }
  }

  return(dcsbm_B)
}
