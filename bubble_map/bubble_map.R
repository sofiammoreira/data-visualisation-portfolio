# Here are the commands to build a bubble map

# Packages
library(tidyverse)
library(spData)
library(sf)
library(tmap)

# Load csv file
# Data retrieved from: (https://2023census-statsnz.hub.arcgis.com/datasets/StatsNZ::2023-census-population-change-by-age-group-and-rc/explore?layer=1&location=-4.363859%2C0.000000%2C1.81)
df_data <- read.csv("2023_Census_population_change_by_age_group.csv")

# Select important columns
df_2023 <- data.frame(
  Name = as.character(df_data$Regional.council..RC..2023.name.no.macrons),
  Pop_15 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2023..Measure..Count..Var1..Age..life.cycle.groups...Under.15.years.),
  Pop_15_29 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2023..Measure..Count..Var1..Age..life.cycle.groups...15.29.years.),
  Pop_30_64 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2023..Measure..Count..Var1..Age..life.cycle.groups...30.64.years.),
  Pop_65 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2023..Measure..Count..Var1..Age..life.cycle.groups...65.years.and.over.)
)

df_2023$Name <- gsub(" Region","",df_2023$Name)
df_2023$Name <- gsub("Manawatu-Whanganui","Manawatu-Wanganui",df_2023$Name)

# Add columns with population by age groups to the nz data and parse it
nz_age_group <- nz %>%
  left_join(df_2023, by = "Name") %>%
  pivot_longer(cols = 7:10,
               names_to = "Population_group",
               values_to = "Population_group_number")

# Plot
tm_shape(nz_age_group) +
  tm_borders(fill = "gray",
             fill_alpha = 0.2) +
  tm_title("Change in population by age group,by regional council in 2023",
           position = tm_pos_out("center", "top")) +
  tm_facets_hstack(by = "Population_group") +
  tm_layout(panel.labels = c("Under 15", "15-29",
                             "30-64", "65 and over")) +
  tm_bubbles(size = "Population_group_number",
             fill = "Population_group_number",
             fill_alpha = 0.9,
             fill.scale = tm_scale_intervals(n = 4, values = "brewer.reds"),
             fill.legend = tm_legend_combine("size"),
             size.legend = tm_legend(title = "Population",
                                     position = tm_pos_out("right","center")))
