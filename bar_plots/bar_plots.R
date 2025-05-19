# Here are the commands to build bar plots

# Packages
library(tidyverse)

# Load csv file
# Data retrieved from: (https://www.stats.govt.nz/information-releases/environmental-economic-accounts-data-to-2022/)
df_data <- read.csv("environmental_taxes.csv")

# Parse data
df_data_longer <- df_data %>%
  pivot_longer(cols = 2:5,
               names_to = "Type",
               values_to = "Value") %>%
  mutate(Value = replace(Value, which(Value == "NaN"), NA),
         Value = gsub(",","",Value),
         Value = as.numeric(Value),
         Billions = Value/1e9)

# Plots
ggplot(df_data_longer, aes(x = Year.ended.March, y = Billions, fill = Type)) +
  geom_col() +
  facet_grid(rows = vars(Type),
             scales = "free_y") +
  scale_fill_manual(values = c("Energy" = "#0a526aff",
                               "Pollution" = "#cca229ff",
                               "Resources" = "#a54547ff",
                               "Transport" = "#2e2e53ff")) +
  labs(x = "Year", y = "$ (billions)") +
  ggtitle("Environmental taxes by tax base ($), year ended March 1999-2022") +
  scale_x_continuous(breaks = df_data_longer$Year.ended.March) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 9, colour = "black"), 
        axis.text.x = element_text(angle = 30, hjust=1, size = 8, colour = "black"),
        axis.title = element_text(size = 9, colour = "black", face = "bold"),
        legend.position="none",
        strip.text = element_text(face = "bold"))
