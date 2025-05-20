# Here are the commands to build heatmaps

# Packages
library(tidyverse)
library(scales)

# Load csv file
# Data retrieved from: (https://2023census-statsnz.hub.arcgis.com/datasets/StatsNZ::2023-census-population-change-by-age-group-and-rc/explore?layer=1&location=-4.363859%2C0.000000%2C1.81)
df_data <- read.csv("2023_Census_population_change_by_age_group.csv")

# Create a dataframe for each year
df_2013 <- data.frame(
  Name = as.character(df_data$Regional.council..RC..2023.name.no.macrons),
  Pop_15 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2013..Measure..Count..Var1..Age..life.cycle.groups...Under.15.years.),
  Pop_15_29 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2013..Measure..Count..Var1..Age..life.cycle.groups...15.29.years.),
  Pop_30_64 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2013..Measure..Count..Var1..Age..life.cycle.groups...30.64.years.),
  Pop_65 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2013..Measure..Count..Var1..Age..life.cycle.groups...65.years.and.over.),
  Year = 2013
)

df_2018 <- data.frame(
  Name = as.character(df_data$Regional.council..RC..2023.name.no.macrons),
  Pop_15 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2018..Measure..Count..Var1..Age..life.cycle.groups...Under.15.years.),
  Pop_15_29 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2018..Measure..Count..Var1..Age..life.cycle.groups...15.29.years.),
  Pop_30_64 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2018..Measure..Count..Var1..Age..life.cycle.groups...30.64.years.),
  Pop_65 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2018..Measure..Count..Var1..Age..life.cycle.groups...65.years.and.over.),
  Year = 2018
)

df_2023 <- data.frame(
  Name = as.character(df_data$Regional.council..RC..2023.name.no.macrons),
  Pop_15 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2023..Measure..Count..Var1..Age..life.cycle.groups...Under.15.years.),
  Pop_15_29 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2023..Measure..Count..Var1..Age..life.cycle.groups...15.29.years.),
  Pop_30_64 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2023..Measure..Count..Var1..Age..life.cycle.groups...30.64.years.),
  Pop_65 = as.numeric(df_data$Subject.pop..Census.usually.resident.population..Year..2023..Measure..Count..Var1..Age..life.cycle.groups...65.years.and.over.),
  Year = 2023
)

# Combine all tables
df_all <- rbind(df_2013,df_2018,df_2023)

# Parse data
df_parsed <- df_all %>%
  pivot_longer(cols = 2:5,
               names_to = "Population_group",
               values_to = "Population_group_number") %>%
  mutate(Name = gsub(" Region","",Name))

# Heatmaps
Pop_names = c("Pop_15" = "Under 15",
              "Pop_15_29" = "15-29",
              "Pop_30_64" = "30-64",
              "Pop_65" = "65 and over")

df_parsed$Name <- factor(df_parsed$Name, levels = rev(sort(unique(df_parsed$Name))))

ggplot(df_parsed, aes(x = as.factor(Year), y = Name, fill = Population_group_number)) +
  geom_tile(colour = "white", linewidth = 1) +
  facet_grid(~Population_group,
             labeller = as_labeller(Pop_names)) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Population",
                       limits = c(0, 800000),
                       labels = label_number(scale = 1, accuracy = 1)) +
  labs(x = "Year", y = "Region", title = "Change in population by age group, by regional council") +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 9, colour = "black"), 
        axis.title = element_text(size = 9, colour = "black", face = "bold"),
        strip.text = element_text(face = "bold"))
