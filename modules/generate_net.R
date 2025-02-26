

generate_s_net_R_old <- function(network_type,
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
      # factor_2_assort = NULL
      #
      # deg_seq_net = deg_seq_net %>%
      #   mutate(block = case_when((risk == "MSM") ~ 1,
      #                            (risk == "MSMandIDU") ~ 2)) %>%
      #   arrange(block)

      factor_2_assort = simObj$trans_params$Assortativity_MSM %>% filter(`Age Category` == "hiv_status") %>% pull(Value)

      deg_seq_net = deg_seq_net %>%
        mutate(block = case_when((risk == "MSM" & hiv_status == "0") ~ 1,
                                 (risk == "MSM" & hiv_status == "1") ~ 2,
                                 (risk == "MSMandIDU" & hiv_status == "0") ~ 3,
                                 (risk == "MSMandIDU" & hiv_status == "1") ~ 4)) %>%
        arrange(block)
    }
    # deg_seq_net = deg_seq_net %>%
    #   mutate(networkx_id = c(1:(n())))

    deg_seq_net$num_partners = deg_seq_net$MSM_partners
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
      # factor_2_assort = NULL
      #
      # deg_seq_net = deg_seq_net %>%
      #   mutate(block = case_when((gender == "female" & risk == "other") ~ 1,
      #                            (gender == "female" & risk == "IDU") ~ 2,
      #                            (gender == "male" & risk == "other") ~ 3,
      #                            (gender == "male" & risk == "IDU") ~ 4
      #   )) %>%
      #   arrange(block)

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
    }
    # deg_seq_net = deg_seq_net %>%
    #   mutate(networkx_id = c(1:(n())))

    deg_seq_net$num_partners = deg_seq_net$HET_partners

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
      # factor_2_assort = NULL
      #
      # deg_seq_net = deg_seq_net %>%
      #   mutate(block = case_when((gender == "female" & risk == "other") ~ 1,
      #                            (gender == "female" & risk == "IDU") ~ 2,
      #                            (gender == "male" & risk == "MSM") ~ 3,
      #                            (gender == "male" & risk == "MSMandIDU") ~ 4
      #   )) %>%
      #   arrange(block)

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
    }

    # if (nrow(deg_seq_net) > 0) {
    #   deg_seq_net = deg_seq_net %>%
    #     mutate(networkx_id = c(1:(n())))
    # }

    deg_seq_net$num_partners = deg_seq_net$MSMW_partners

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
    # deg_seq_net = deg_seq_net %>%
    #   mutate(networkx_id = c(1:(n())))

    deg_seq_net$num_partners = deg_seq_net$IDU_partners

  } else {
    print("ERROR")
  }

  dcsbm_B_unnorm = generate_dcsbm_b_matrix(network_type,
                                           init_net,
                                           factor_1_assort,
                                           factor_2_assort) %>%
    as.matrix()

  num_blocks = nrow(dcsbm_B_unnorm)

  # Duplicate rows based on 'num_partners' column
  deg_seq_net = deg_seq_net %>%
    filter(num_partners > 0)

  if (nrow(deg_seq_net) > 0) {
    deg_seq_net_expanded <- deg_seq_net[rep(1:nrow(deg_seq_net), deg_seq_net$num_partners), ]
    deg_seq_net_expanded_rand <- sample_n(deg_seq_net_expanded, nrow(deg_seq_net_expanded), replace = FALSE)
    deg_seq_net_expanded_rand$index = c(1:nrow(deg_seq_net_expanded_rand))

    block_sizes = tabulate(deg_seq_net_expanded_rand$block, nbins = num_blocks)
    net.edgelist = NULL

    if (network_type %in% c("MSM", "IDU")) {
      for (i in c(1:num_blocks)) {
        linkage_prob = dcsbm_B_unnorm[i,i]
        num_links = floor(linkage_prob*block_sizes[i])
        if (num_links > 0) {
          i_links = sample_n(deg_seq_net_expanded_rand %>% filter(block == i),
                             num_links,
                             replace = FALSE)
          edgelist_ii = tibble(source = i_links[c(1:floor(nrow(i_links)/2)), ] %>% pull(id),
                               target = i_links[c((floor(nrow(i_links)/2)+1):(2*(floor(nrow(i_links)/2)))), ] %>% pull(id))
          deg_seq_net_expanded_rand = deg_seq_net_expanded_rand %>%
            filter(!index %in% i_links$index)

          net.edgelist = bind_rows(net.edgelist, edgelist_ii)
        }
      }

      if (nrow(deg_seq_net_expanded_rand) > 2) {
        edgelist_ij = tibble(source = deg_seq_net_expanded_rand[c(1:floor(nrow(deg_seq_net_expanded_rand)/2)), ] %>% pull(id),
                             target = deg_seq_net_expanded_rand[c((floor(nrow(deg_seq_net_expanded_rand)/2)+1):(2*(floor(nrow(deg_seq_net_expanded_rand)/2)))), ] %>% pull(id))
        net.edgelist = bind_rows(net.edgelist, edgelist_ij)
      }
    }

    if (network_type %in% c("MSMW", "HET")) {
      for (i in c(1:(num_blocks/2))) {
        linkage_prob = dcsbm_B_unnorm[i,i + num_blocks/2]
        num_links = min(floor(linkage_prob*block_sizes[i]), block_sizes[i+ num_blocks/2])

        if (num_links > 0) {
          i_links = sample_n(deg_seq_net_expanded_rand %>% filter(block == i),
                             num_links,
                             replace = FALSE)
          j_links = sample_n(deg_seq_net_expanded_rand %>% filter(block == (i + num_blocks/2)),
                             num_links,
                             replace = FALSE)
          edgelist_ij = tibble(source = i_links %>% pull(id),
                               target = j_links %>% pull(id))
          deg_seq_net_expanded_rand = deg_seq_net_expanded_rand %>%
            filter(!index %in% i_links$index)
          deg_seq_net_expanded_rand = deg_seq_net_expanded_rand %>%
            filter(!index %in% j_links$index)

          net.edgelist = bind_rows(net.edgelist, edgelist_ij)
        }
      }

      if (nrow(deg_seq_net_expanded_rand) > 2) {

        deg_seq_net_expanded_rand_F = deg_seq_net_expanded_rand %>%
          filter(gender == "female")
        deg_seq_net_expanded_rand_M = deg_seq_net_expanded_rand %>%
          filter(gender == "male")
        num_edge_pairs = min(nrow(deg_seq_net_expanded_rand_F),
                             nrow(deg_seq_net_expanded_rand_M))
        if (num_edge_pairs > 0) {
          edgelist_ij = tibble(source = deg_seq_net_expanded_rand_F[c(1:num_edge_pairs), ] %>% pull(id),
                               target = deg_seq_net_expanded_rand_M[c(1:num_edge_pairs), ] %>% pull(id))
          net.edgelist = bind_rows(net.edgelist, edgelist_ij)
        }
      }
    }

    #dcsbm_pi = as.numeric(tabulate(deg_seq_net$block, nbins = (dcsbm_B %>% nrow())))/nrow(deg_seq_net)

    ########
    #temp = as.numeric(tabulate(deg_seq_net$block, nbins = (dcsbm_B %>% nrow())))
    #dcsbm_B_dem = temp %*% t(temp)
    #diag(dcsbm_B_dem) = choose(temp, 2)

    #dcsbm_B = dcsbm_B_unnorm/dcsbm_B_dem
    ########

    # if (nrow(deg_seq_net) > 0 & sum(as.numeric(dcsbm_theta)) > 4) {
    #   error_flag = TRUE
    #   while(error_flag) {
    #     tryCatch({
    #       g = dcsbm(
    #         theta = as.numeric(dcsbm_theta),
    #         B = dcsbm_B,
    #         expected_density = (sum(dcsbm_theta)/2 - 1)/choose(length(dcsbm_theta),2),
    #         pi = dcsbm_pi,
    #         sort_nodes = TRUE,
    #         poisson_edges = FALSE,
    #         allow_self_loops = FALSE
    #       )
    #       error_flag = FALSE
    #     }, error = function(e) {error_flag = TRUE})
    #   }
    #
    #     edgelist <- sample_edgelist(g)
    if (!is.null(net.edgelist)) {
      if (nrow(net.edgelist) > 0) {
      # if (nrow(edgelist) > 0) {
      # net.edgelist = left_join(edgelist, deg_seq_net %>% select(id, networkx_id),
      #                          by = join_by(from == networkx_id)) %>%
      #   select(-from) %>%
      #   rename(source = id) %>%
      #   left_join(deg_seq_net %>% select(id, networkx_id),
      #             by = join_by(to == networkx_id)) %>%
      #   select(-to) %>%
      #   rename(target = id)

      # Temp = left_join(edgelist, deg_seq_net %>% select(id, networkx_id),
      #                         by = join_by(from == networkx_id)) %>%
      #  rename(source = id) %>%
      #  left_join(deg_seq_net %>% select(id, networkx_id),
      #            by = join_by(to == networkx_id)) %>%
      #  rename(target = id)
      } else {
        net.edgelist = tibble(source = NULL,
                             target = NULL)
      }
    } else {
      net.edgelist = tibble(source = NULL,
                            target = NULL)
    }
    return(net.edgelist)
  } else {
    net.edgelist = tibble(source = NULL,
                          target = NULL)
    return(net.edgelist)
  }
}

generate_dcsbm_b_matrix_old <- function(network_type,
                                  init_net,
                                  factor_1_assort,
                                  factor_2_assort) {

  if (network_type %in% c("MSM", "HET", "MSMW")) {
    if (TRUE) {
    #if (init_net == TRUE) {

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
