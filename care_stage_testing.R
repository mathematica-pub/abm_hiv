library(readxl)
library(tidyverse)

file_loc_input = "/Users/ravigoyal/Dropbox/County HRSA Grant/Data_requests/San Diego/Data_received_documentation/Stage_of_Care/All_Stages_Initialization_Table_realloc.csv"

SoC.df = read_csv(file = file_loc_input)

SoC.df %>% group_by(Stage) %>%
  summarise(total_stagee_prob = sum(percent))

