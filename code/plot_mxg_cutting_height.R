# plot_mxg_cutting_height.R
# By: Bryan Petersen
# Date: 2024-01-14
# Purpose: Plot the cutting height from observations at the commercial fields in April 2021
# Input: data/external/cutting_height_obs.csv
# Output: visuals/cutting_height/obs_boxplot.jpg
#         visuals/cutting_height/obs_resid_panel.jpg
#         visuals/cutting_height/obs_baled_boxplot.jpg
#         visuals/cutting_height/obs_field_boxplot.jpg
#         visuals/cutting_height/obs_composite.jpg

# Load libraries
library(tidyverse)
library(ggResidpanel)
library(patchwork)

# Create the visuals/cutting_height directory if it doesn't exist
dir.create("visuals/cutting_height", showWarnings = FALSE, recursive = TRUE)

# Read data
data_tbl <- read_csv(file = "data/external/cutting_height_obs.csv") %>% 
    mutate(baled = if_else(baled, "Baled", "Not baled"))

# Plot the cutting height with a single boxplot for all the observations
p1 <- data_tbl %>%
    ggplot(aes(x = "", y = stem_height)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(size = 2, width = 0.2, height = 0, color = "darkgrey", alpha = 0.8) +
    labs(x = NULL, y = "Cutting height, cm") +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"))

# Save the plot
ggsave(filename = "visuals/cutting_height/obs_boxplot.jgp", plot = p1, width = 8, height = 8, units = "in", dpi = 300)

# Fit linear model to the data
stubble_height_mod <- lm(stem_height ~ baled, data = data_tbl)

# Diagnostic plot
p2 <- resid_panel(stubble_height_mod)

# Save the plot
ggsave(filename = "visuals/cutting_height/obs_resid_panel.jpg", plot = p2, width = 8, height = 8, units = "in", dpi = 300)

significance_tbl <- data_tbl %>% 
    t.test(stem_height ~ baled, data = .) %>% 
    broom::tidy()

# Plot the cutting height with a boxplot for the baled and not baled sections
p3 <- data_tbl %>%
    mutate(baled = factor(baled, levels = c("Not baled", "Baled"))) %>%
    ggplot(aes(x = baled, y = stem_height)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(size = 2, width = 0.2, height = 0, color = "darkgrey", alpha = 0.8) +
    geom_segment(x = 1, xend = 2, y = 49.5, yend = 49.5) +
    geom_text(x = 1.5, y = 50, label = "*", size = 8) +
    labs(x = NULL, y = "Cutting height, cm") +
    theme_bw() +
    theme(axis.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"))

# Save the plot
ggsave(filename = "visuals/cutting_height/obs_baled_boxplot.jpg", plot = p3, width = 8, height = 8, units = "in", dpi = 300)



# Create pretty labels for the fields
pretty_labels <- c("dahlen" = "Dahlen",
                   "freddies" = "Freddies",
                   "solomon" = "Bell Trust",
                   "ui_melrose" = "UofI Melrose")

# Plot the cutting with a boxplot for each field
p4 <- data_tbl %>%
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
ggsave(filename = "visuals/cutting_height/obs_field_boxplot.jpg", plot = p2, width = 8, height = 8, units = "in", dpi = 300)

# Combine the single boxplot and baled/not baled boxplot
composite_plot <- p1 + p3 + plot_annotation(tag_levels = "A") + plot_layout(axes = "collect") & 
    theme(plot.tag = element_text(size = 12, face = "bold"))

# Save the composite plot
ggsave(filename = "visuals/cutting_height/obs_composite.jpg", plot = composite_plot, width = 8, height = 8, units = "in", dpi = 300)




