

library(readxl)
library(tidyverse)
library(ggplot2)

origin = "/Users/ravigoyal/Dropbox/Academic/Research/Projects/ASPIRE/MigrationTables_20241101.xlsx"

migration <- read_excel(origin, "MDInOutNbhdWide", "A1:AH2206")
reside <- read_excel(origin, "CharacComboReside", "A1:U2206")

migration_sum = migration %>%
  group_by(interval) %>%
  summarise(across(starts_with("migrate"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(across(starts_with("migrate"), mean, na.rm = TRUE))

migration_sum_long = migration_sum %>% pivot_longer(
  cols = starts_with("migrate"),
  names_to = c("In_out", "Region"),
  names_pattern = "migrate_(.*)_R(.*)",
  values_to = "migration"
)

reside_sum = reside %>%
  group_by(interval) %>%
  summarise(across(starts_with("reside"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(across(starts_with("reside"), mean, na.rm = TRUE))

reside_sum_long = reside_sum %>% pivot_longer(
  cols = starts_with("reside"),
  names_to = c("Region"),
  names_pattern = "reside_R(.*)",
  values_to = "reside"
)

migration.df = left_join(migration_sum_long, reside_sum_long, by = "Region") %>%
  mutate(percent = migration / reside) %>%
  filter(Region != "NA")

region_names.df = data.frame(
  num = c(1:13),
  name = c("South Dade/Homestead",
           "Kendall",
           "Westchester/W Dade",
           "Coral Gables/Kendall",
           "Brownsville/Coral Gables/Coconut Grove",
           "Coral Gables/Coconut Grove/Key Biscayne",
           "Doral/Miami Springs/Sunset",
           "Miami Shores/Morningside",
           "Hialeah/Miami Lakes",
           "Opalocka/Miami Gardens/Westview",
           "North Miami/North Miami Beach",
           "Aventura/Miami Beach",
           "Downtown/Little Havana/Liberty City/Little Haiti/Overtown")
)

region_names.df = data.frame(
  Region= as.character(c(1:13)),
  Name = c("Homestead",
           "Kendall",
           "Westchester",
           "Kendall",
           "Brownsville",
           "Key Biscayne",
           "Doral",
           "Miami Shores",
           "Hialeah",
           "Opalocka",
           "North Miami",
           "Aventura",
           "Downtown")
)

migration.df = left_join(migration.df, region_names.df, by = "Region")

ggplot(migration.df, aes(x = Name, y = percent, fill = In_out)) +
  geom_bar(stat = "identity", position = "dodge") + # Dodge separates bars by migration type
  labs(
    title = "Percentage of Migration by Region",
    x = "",
    y = "Percentage of PWH",
    fill = "Migration Type"
  ) +
  scale_fill_manual(
    values = c("in" = "steelblue", "out" = "firebrick"), # Custom colors for bars
    labels = c("in" = "Migrate In", "out" = "Migrate Out") # Change legend labels
  ) +
  theme_minimal() + # Clean minimal theme
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels by 45 degrees
  )

