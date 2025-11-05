

homophily_check = function(simObj) {

  Combpop.df = bind_rows(simObj$popdf, simObj$negpopdf)

  TEMP = simObj$networks$MSM %>% left_join(Combpop.df %>% select(id, risk),
                                    by = c("ID1" = "id"),
                                    relationship = "many-to-one") %>%
    left_join(Combpop.df %>% select(id, risk),
              by = c("ID2" = "id"),
              relationship = "many-to-one")
  total = table(TEMP$risk.x, TEMP$risk.y)[2,] %>% sum()
  MSM_val = (table(TEMP$risk.x, TEMP$risk.y)/total * 100)[2,2]
  print(paste("MSM: ", MSM_val), sep = "")

  TEMP = simObj$networks$HET %>% left_join(Combpop.df %>% select(id, risk),
                                           by = c("ID1" = "id"),
                                           relationship = "many-to-one") %>%
    left_join(Combpop.df %>% select(id, risk),
              by = c("ID2" = "id"),
              relationship = "many-to-one")
  total = table(TEMP$risk.x, TEMP$risk.y)[2,] %>% sum()
  HET_val = (table(TEMP$risk.x, TEMP$risk.y)/total * 100)[2,2]
  print(paste("HET: ", HET_val), sep = "")

  return(c(MSM_val, HET_val))

}


homophily_check_2 = function(simObj) {

  Combpop.df = bind_rows(simObj$popdf, simObj$negpopdf)

  TEMP = net.edgelist %>% left_join(Combpop.df %>% select(id, risk),
                                           by = c("source" = "id"),
                                           relationship = "many-to-one") %>%
    left_join(Combpop.df %>% select(id, risk),
              by = c("target" = "id"),
              relationship = "many-to-one")
  total = table(TEMP$risk.x, TEMP$risk.y)[1,] %>% sum()
  print(paste("MSM: ", (table(TEMP$risk.x, TEMP$risk.y)/total * 100)[1,1]), sep = "")
}
