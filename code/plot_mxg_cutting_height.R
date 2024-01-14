# plot_mxg_cutting_height.R
# By: Bryan Petersen
# Date: 2024-01-14
# Purpose: Plot the cutting height from observations at the commercial fields in April 2021
# Input: data/external/cutting_height_obs.csv
# Output: visuals/cutting_height/obs_boxplot.png
#         visuals/cutting_height/obs_boxplot.tiff
#         visuals/cutting_height/obs_field_boxplot.png
#         visuals/cutting_height/obs_field_boxplot.tiff

# Load libraries
library(tidyverse)

# Create the visuals/cutting_height directory if it doesn't exist
dir.create("visuals/cutting_height", showWarnings = FALSE, recursive = TRUE)

# Read data
data_tbl <- read_csv(file = "data/external/cutting_height_obs.csv")

# Plot the cutting height with a single boxplot for all the observations
p1 <- data_tbl %>%
    ggplot(aes(x = "", y = stem_height)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(size = 2) +
    labs(x = NULL, y = "Cutting height, cm") +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"))

# Save the plot
ggsave(filename = "visuals/cutting_height/obs_boxplot.png", plot = p1, width = 8, height = 8, units = "in", dpi = 300)
ggsave(filename = "visuals/cutting_height/obs_boxplot.tiff", plot = p1, width = 8, height = 8, units = "in", dpi = 300)

pretty_labels <- c("dahlen" = "Dahlen",
                   "freddies" = "Freddies",
                   "solomon" = "Bell Trust",
                   "ui_melrose" = "UofI Melrose")

# Plot the cutting with a boxplot for each field
p2 <- data_tbl %>%
    mutate(field_name = factor(field_name)) %>% 
    ggplot(aes(x = field_name, y = stem_height)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(size = 2) +
    labs(x = "Field", y = "Cutting height, cm") +
    scale_x_discrete(labels = pretty_labels) +
    theme_bw() +
    theme(axis.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"))

# Save the plot
ggsave(filename = "visuals/cutting_height/obs_field_boxplot.png", plot = p2, width = 8, height = 8, units = "in", dpi = 300)
ggsave(filename = "visuals/cutting_height/obs_field_boxplot.tiff", plot = p2, width = 8, height = 8, units = "in", dpi = 300)
