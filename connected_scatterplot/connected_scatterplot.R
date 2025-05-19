# Here are the commands to build a connect scatterplot

# Packages
library(tidyverse)

# Load csv file
# Data retrieved from: (https://www.stats.govt.nz/topics/agriculture/)
df_data <- read.csv("agriculture.csv")

# Parse data
df_data_longer <- df_data %>%
  pivot_longer(cols = starts_with(match = "Total"),
               names_to = "Cattle",
               values_to = "Number") %>%
  mutate(Number_millions = Number/1e6)

# Plot
ggplot(df_data_longer, aes(x = Category, y = Number_millions, colour = Cattle)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(labels = c("Beef","Dairy"),
                      values = c("#cca229ff","#0a526aff")) +
  labs(x = "Year (at 30 June)", y = "Number (millions)") +
  ggtitle("Dairy cattle and beef cattle numbers, 2002-2024") +
  coord_cartesian(ylim = c(0, 8)) +
  scale_x_continuous(breaks = df_data_longer$Category) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 9, colour = "black"), 
        axis.text.x = element_text(angle = 30, hjust=1, size = 9, colour = "black"),
        axis.title = element_text(size = 9, colour = "black", face = "bold"),
        legend.title = element_text(size = 9,face = "bold"),
        legend.key = element_rect(linewidth = 0.1),
        legend.position = c(0.01, 0.85), 
        legend.justification='left')
  
  
