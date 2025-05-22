# Here are the commands to build a scatter plot with a linear trend

# Packages
library(tidyverse)
library(ggpmisc)
library(ggpubr)

# Load csv file
# Data retrieved from: (https://www.stats.govt.nz/indicators/greenhouse-gas-concentrations/)
df_data <- read.csv("carbon-dioxide-(co₂)-concentrations-at-baring-head,-wellington,-1972–2022.csv")

# Use data from March of each year
df_march <- df_data %>%
  filter(grepl("Mar", Date, ignore.case = TRUE),
         Mean != "NaN") %>%
  mutate(Date = gsub("Mar-","",Date),
         Date = as.numeric(Date))

# Plot
ggplot(df_march,aes(x = Date, y = Mean)) +
  geom_point(colour="#69b3a2ff") +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#333333ff",
              fill="#69b3a2",
              linewidth = 0.5) +
  stat_regline_equation(aes(label = ..eq.label..),
                        label.y = 400) +
  stat_poly_eq(aes(label = ..rr.label..),
               rr.digits = 3) +
  labs(x = "Year", y = "Concentration (ppm)",
       title = expression(bold(paste("Carbon dioxide ", (CO[2])," concentrations at Baring Head, Wellington, 1972-2022")))) +
  theme(legend.key = element_blank(),
        panel.background = element_rect(fill='transparent'),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(colour = "black"),
        axis.title = element_text(face = "bold"))
