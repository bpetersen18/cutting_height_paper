#!/usr/bin/env Rscript
# plot_mxg_cutting_height.R
# By: Bryan Petersen
# Date: 2023-08-24

# Load libraries
library(tidyverse)

# Read data
data_tbl <- read_csv(file = "data/raw/cutting_height_obs.csv")

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
ggsave(filename = "visuals/cutting_height_obs.png", plot = p1, width = 8, height = 8, units = "in", dpi = 300)
ggsave(filename = "visuals/cutting_height_obs.tiff", plot = p1, width = 8, height = 8, units = "in", dpi = 300)

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
ggsave(filename = "visuals/cutting_height_obs_field.png", plot = p2, width = 8, height = 8, units = "in", dpi = 300)
ggsave(filename = "visuals/cutting_height_obs_field.tiff", plot = p2, width = 8, height = 8, units = "in", dpi = 300)
