# Here are the commands to build violin + boxplots

# Packages
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

# Load csv file
# Data retrieved from: (https://www.stats.govt.nz/large-datasets/csv-files-for-download/)
df_data <- read.csv("food-price-index-september-2023-weighted-average-prices.csv")

### Fruits
# Filter rows
df_fruits <- df_data %>%
  filter(grepl("Oranges|Banana|Apples|Avocado|Pears|Kiwifruit", Series_title_1, ignore.case = TRUE)) %>%
  mutate(Series_title_1 = gsub(", 1kg","",Series_title_1),
         Year = gsub("\\..*","",Period))

# Plot
A <- ggplot(df_fruits, aes(x = Series_title_1, y = Data_value)) +
  geom_violin(alpha=0.5,scale = "width",
              aes(x = Series_title_1,fill = Series_title_1)) +
  geom_boxplot(width=0.2,alpha=0.2, aes(fill = Series_title_1),
               outlier.shape = NA) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Product", y = "Price ($)", title = "Fruits (1 kg)") +
  coord_cartesian(ylim = c(0, 32)) +
  theme(legend.position="none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 9, colour = "black"), 
        axis.title = element_text(size = 9, colour = "black", face = "bold" ))

### Vegetables
# Filter rows
df_vegetables <- df_data %>%
  filter(grepl("Lettuce|Broccoli|Cabbage|Carrots|Potatoes|Cauliflower", Series_title_1, ignore.case = TRUE)) %>%
  mutate(Series_title_1 = gsub(", 1kg","",Series_title_1))

# Plot
B <- ggplot(df_vegetables, aes(x = Series_title_1, y = Data_value)) +
  geom_violin(alpha=0.5,scale = "width",
              aes(x = Series_title_1,fill = Series_title_1)) +
  geom_boxplot(width=0.2,alpha=0.2, aes(fill = Series_title_1),
               outlier.shape = NA) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Product", y = "Price ($)", title = "Vegetables (1 kg)") +
  coord_cartesian(ylim = c(0, 15)) +
  theme(legend.position="none",
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 9, colour = "black"), 
        axis.title = element_text(size = 9, colour = "black", face = "bold" ))

### Arrange plots
ggarrange(A,NULL,B,
          nrow = 1, widths = c(1, 0.07, 1),
          heights = c(1, 0.07, 1),
          labels = c("A", "", "B"),
          font.label = list(size = 12))


