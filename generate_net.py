
import networkx as nx
import pandas as pd
import numpy as np

def generate_s_net_py(
  network_type = "HET",
  deg_seq_1 = [],
  deg_seq_2 = [],
  num_assort_factors = 0,
  node_ids = None,
  factor_1_list = None,
  factor_1_assort = 0,
  factor_2_list = None,
  factor_2_assort = 0):
    
    if (network_type == "HET"):
      g = nx.bipartite.configuration_model(
        aseq = deg_seq_1, 
        bseq = deg_seq_2, 
        create_using=None, 
        seed=None
      )
    elif (network_type == "MSMW"):
      g = nx.bipartite.configuration_model(
        aseq = deg_seq_1, 
        bseq = deg_seq_2, 
        create_using=None, 
        seed=None
      )      
    else:
      g = nx.configuration_model(
        deg_sequence= deg_seq_1,
        create_using=None, 
        seed=None
      )
    
    if (num_assort_factors > 0):
      if (num_assort_factors >= 1):
        factor_1_dict = dict(zip(node_ids, factor_1_list))
        nx.set_node_attributes(g, factor_1_dict, "factor_1")
        factor_1_assort_cur = nx.attribute_assortativity_coefficient(g, "factor_1")
        assort_obj_cur = pow(abs(factor_1_assort_cur - factor_1_assort),2)
      if (num_assort_factors == 2):
        factor_2_dict = dict(zip(node_ids, factor_2_list))
        nx.set_node_attributes(g, factor_2_dict, "factor_2")
        factor_2_assort_cur = nx.attribute_assortativity_coefficient(g, "factor_2")
        assort_obj_cur = assort_obj_cur + pow(abs(factor_2_assort_cur - factor_2_assort),2)
  
      print('factor_1_assort_cur: ', factor_1_assort_cur)
      if (num_assort_factors == 2):
        print('factor_2_assort_cur: ', factor_2_assort_cur)
        
      for i in range(10):
      
        g_TEMP = nx.double_edge_swap(g,
          nswap=10, 
          max_tries=100, 
          seed=None
        )
      
        if (num_assort_factors >= 1):
          factor_1_assort_TEMP = nx.attribute_assortativity_coefficient(g_TEMP, "factor_1")
          assort_obj_TEMP = pow(abs(factor_1_assort_TEMP - factor_1_assort),2)
        if (num_assort_factors == 2):
          factor_2_assort_TEMP = nx.attribute_assortativity_coefficient(g_TEMP, "factor_2")
          assort_obj_TEMP = assort_obj_TEMP + pow(abs(factor_2_assort_TEMP - factor_2_assort),2)

        if (assort_obj_TEMP < assort_obj_cur):
          #print('factor_1_assort_cur: ', factor_1_assort_cur)
          #print('factor_1_assort_TEMP: ', factor_1_assort_TEMP)
          #print('assort_obj_cur: ', assort_obj_cur)
          #print('assort_obj_TEMP: ', assort_obj_TEMP)
          assort_obj_cur = assort_obj_TEMP
          factor_1_assort_cur = factor_1_assort_TEMP
          if (num_assort_factors == 2):
            factor_2_assort_cur = factor_2_assort_TEMP
          g = g_TEMP
    
      print('factor_1_assort_cur: ', factor_1_assort_cur)
      if (num_assort_factors == 2):
        print('factor_2_assort_cur: ', factor_2_assort_cur)
        
    g_df = nx.to_pandas_edgelist(g)
    
    return g_df
