#!/usr/bin/env Rscript
# model_mxg_stem_partition.R
# By: Bryan Petersen
# Date: 2023-07-25
# Purpose: Model the stem mass partitioning in miscanthus using the data from SERF
# Input: data/derived/serf_segment_data.csv
# Output: data/derived/mxg_stem_model.rds
#         visuals/stem_count_boxplot.png
#         visuals/stem_mass_vs_segment_nrate_factor.png
#         visuals/stem_cumulative_mass_vs_segment_nrate_factor.png
#         visuals/stem_mass_fraction_vs_segment_nrate_factor.png
#         visuals/stem_cumulative_mass_fraction_vs_segment_nrate_factor.png
#         visuals/stem_mass_fraction_vs_segment.png
#         visuals/stem_cumulative_mass_fraction_vs_segment.png




# Install nlraa
install.packages("nlraa", .libPaths(), repos = "https://mirror.las.iastate.edu/CRAN/")

# Load libraries
library(tidyverse)
library(rstatix)
library(nlraa)
library(nlme)
library(emmeans)

# Read csv file
serf_segment_data <- read_csv(file = "data/derived/serf_segment_data.csv")  %>% 
    mutate(nrate_factor = as.factor(nrate))

## ----------------------------------------------------- ##
## Test if there is difference in density between nrates ##
## ----------------------------------------------------- ##

# Get just stem count for each plot
stem_count <- serf_segment_data %>% 
    group_by(plot, nrate_factor) %>% 
    summarise(stem_count = mean(stem_count)) %>% 
    ungroup()

# Calculate summary stats
stem_count_stats <- stem_count %>% 
    group_by(nrate_factor) %>% 
    get_summary_stats(stem_count, type = "mean_sd")

# Create boxplot
p1 <- ggplot(data = stem_count, aes(x = nrate_factor, y = stem_count)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter() +
    labs(x = bquote(bold("Nitrogen, " ~kg%.%Ha^-1)), y = bquote(bold("Stem Count, " ~stem%.%m^-2))) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))

ggsave(plot = p1, filename = "visuals/stem_count_boxplot.png",
       height = 8, width = 8, units = "in")
ggsave(plot = p1, filename = "visuals/stem_count_boxplot.tiff",
       height = 8, width = 8, units = "in")

# Are there outliers
stem_count %>% 
    group_by(nrate_factor) %>% 
    identify_outliers(stem_count)

# There are no extreme outliers

# Check the equality of variances
levene_test_results <- stem_count %>% 
    levene_test(stem_count ~ nrate_factor)

# Levene's test suggests the variances are equal

# Compute t-test
stat_test <- stem_count %>% 
    t_test(stem_count ~ nrate_factor, var.equal = TRUE) %>% 
    add_significance()
stat_test

# Student t-test suggests stem count is not different under the two different nitrogen rates

## ------------------------- ##
## Simple segment mass stats ##
## ------------------------- ##
# Get just segment mass for each plot and segment location
segment_mass <- serf_segment_data %>% 
    group_by(segment, nrate_factor, plot) %>% 
    summarise(segment_mass = mean(segment_mass)) %>% 
    ungroup()

# Calculate summary stats
segment_mass_stats <- segment_mass %>% 
    group_by(nrate_factor) %>% 
    get_summary_stats(segment_mass, type = "mean_ci")

# Are there outliers
segment_mass %>% 
    group_by(nrate_factor) %>% 
    identify_outliers(segment_mass)

# There are no extreme outliers

# Check the equality of variances
levene_test_results <- segment_mass %>% 
    levene_test(segment_mass ~ nrate_factor)

# Levene's test suggests the variances are equal

# Compute t-test
stat_test <- segment_mass %>% 
    t_test(segment_mass ~ nrate_factor, var.equal = TRUE) %>% 
    add_significance()
stat_test

## ---------------------------------- ##
## Simple segment relative mass stats ##
## ---------------------------------- ##
# Get just segment mass for each plot and segment location
segment_rel_mass <- serf_segment_data %>% 
    group_by(segment, nrate_factor, plot) %>% 
    summarise(segment_rel_mass = mean(segment_mass_fraction)) %>% 
    ungroup()

# Calculate summary stats
segment_rel_mass_stats <- segment_rel_mass %>% 
    group_by(nrate_factor) %>% 
    get_summary_stats(segment_rel_mass, type = "mean_ci")

# Are there outliers
segment_rel_mass %>% 
    group_by(nrate_factor) %>% 
    identify_outliers(segment_rel_mass)

# There are no extreme outliers

# Check the equality of variances
levene_test_results <- segment_rel_mass %>% 
    levene_test(segment_rel_mass ~ nrate_factor)

# Levene's test suggests the variances are equal

# Compute t-test
stat_test <- segment_rel_mass %>% 
    t_test(segment_rel_mass ~ nrate_factor, var.equal = TRUE) %>% 
    add_significance()
stat_test

## ----------------------- ##
## Modelling absolute mass ##
## ----------------------- ##
# Mixed model of segment stem mass with segment
# location (4 represents segement from 0 to 4 cm
# and 9 represents from 4 to 8 cm), nitrogen rate (factor), and
# interaction as fixed effects and block as random effects
m1_abs <- lme(segment_mass ~ segment * nrate_factor,
              data = serf_segment_data,
              random = ~ 1 | block)

# Test null hypothesis that the absolute mass distribution does
# not change for different nitrogen rates
anova(m1_abs, type = "sequential")

# Anova indicates there is evidence against the null hypothesis

# Plot model with data
abs_prds <- predict_lme(m1_abs, interval = "conf")
abs_data_prds <- serf_segment_data %>% bind_cols(abs_prds)

p1 <- ggplot(data = abs_data_prds, aes(x = segment, color = nrate_factor, fill = nrate_factor)) +
    geom_point(aes(y = segment_mass/4)) +
    geom_line(aes(y = Estimate/4)) +
    geom_ribbon(aes(ymin = Q2.5/4, ymax = Q97.5/4, color = NULL), alpha = 0.3) +
    labs(x = "Stem Segment, cm", y = bquote(bold("Stem Mass, " ~g%.%cm^-1)), color = bquote(bold("Nitrogen, " ~kg%.%ha^-1)), fill = bquote(bold("Nitrogen, " ~kg%.%ha^-1))) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold", size = 16),
          axis.text = element_text(face = "bold", size = 16),
          legend.text = element_text(face = "bold", size = 16),
          legend.title = element_text(face = "bold", size = 16))
ggsave(plot = p1, filename = "visuals/stem_mass_vs_segment_nrate_factor.png",
       height = 8, width = 8, units = "in")
ggsave(plot = p1, filename = "visuals/stem_mass_vs_segment_nrate_factor.tiff",
       height = 8, width = 8, units = "in")

# Plot cumulative mass
segment_data_cumulative <- serf_segment_data %>%
    group_by(plot) %>%
    mutate(cumulative_segment_mass = cumsum(segment_mass))

prds_cumulative <- abs_data_prds %>%
    group_by(plot) %>% 
    mutate(cumulative_segment_mass = cumsum(Estimate),
           cumulative_lower_bound = cumsum(Q2.5),
           cumulative_upper_bound = cumsum(Q97.5)) %>% 
    ungroup() %>% 
    group_by(nrate_factor, segment) %>% 
    summarise(cumulative_segment_mass = mean(cumulative_segment_mass),
           cumulative_lower_bound = mean(cumulative_lower_bound),
           cumulative_upper_bound = mean(cumulative_upper_bound)) %>% 
    ungroup()

p2 <- ggplot() +
    geom_point(data = segment_data_cumulative, aes(x = segment, y = cumulative_segment_mass, color = nrate_factor)) +
    geom_line(data = prds_cumulative, aes(x = segment, y = cumulative_segment_mass, color = nrate_factor)) +
    geom_ribbon(data = prds_cumulative, aes(x = segment, ymin = cumulative_lower_bound, ymax = cumulative_upper_bound, fill = nrate_factor, color = NULL), alpha = 0.3) +
    labs(x = "Stem Segment, cm", y = "Stem Mass, g") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))

ggsave(plot = p2, filename = "visuals/stem_cumulative_mass_vs_segment_nrate_factor.png",
       height = 8, width = 8, units = "in")
ggsave(plot = p2, filename = "visuals/stem_cumulative_mass_vs_segment_nrate_factor.tiff",
       height = 8, width = 8, units = "in")


## ------------------------ ##
## Modelling factional mass ##
## ------------------------ ##

# Mixed model of fraction of total stem mass with segment
# location (4 represents segement from 0 to 4 cm
# and 9 represents from 4 to 8 cm), nitrogen rate (factor), and
# interaction as fixed effects and block as random effects
m1_rel  <- lme(segment_mass_fraction ~ segment * nrate_factor,
           data = serf_segment_data,
           random = ~ 1 | block)

# Test null hypothesis that the relative mass distribution does
# not change for different nitrogen rates
anova(m1_rel, type = "sequential")

# Anova indicates there is evidence against the null hypothesis

# Plot model with data
prds <- predict_lme(m1_rel, interval = "conf")
serf_segment_data_prds <- serf_segment_data %>% bind_cols(prds)

p3 <- ggplot(data = serf_segment_data_prds, aes(x = segment, color = nrate_factor, fill = nrate_factor)) +
    geom_point(aes(y = segment_mass_fraction*100/4)) +
    geom_line(aes(y = Estimate*100/4)) +
    geom_ribbon(aes(ymin = Q2.5*100/4, ymax = Q97.5*100/4, color = NULL), alpha = 0.3) +
    labs(x = "Stem Segment, cm", y = bquote(bold("Stem Mass Percent, " ~cm^-1)), color = bquote(bold("Nitrogen, " ~kg%.%ha^-1)), fill = bquote(bold("Nitrogen, " ~kg%.%ha^-1))) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold", size = 16),
          axis.text = element_text(face = "bold", size = 16),
          legend.text = element_text(face = "bold", size = 16),
          legend.title = element_text(face = "bold", size = 16))

# Save plot
ggsave(plot = p3, filename = "visuals/stem_mass_fraction_vs_segment_nrate_factor.png",
       height = 8, width = 8, units = "in")
ggsave(plot = p3, filename = "visuals/stem_mass_fraction_vs_segment_nrate_factor.tiff",
       height = 8, width = 8, units = "in")

# Plot cumulative mass fraction
p4 <- serf_segment_data_prds %>% 
    group_by(plot) %>% 
    mutate(cumulative_segment_mass_fraction_obs = cumsum(segment_mass_fraction),
           cumulative_segment_mass_fraction_mod = cumsum(Estimate),
           cumulative_lower_bound = cumsum(Q2.5),
           cumulative_upper_bound = cumsum(Q97.5)) %>% 
    ungroup() %>% 
    ggplot(aes(x = segment, color = nrate_factor, fill = nrate_factor)) +
    geom_point(aes(y = cumulative_segment_mass_fraction_obs)) +
    geom_line(aes(y = cumulative_segment_mass_fraction_mod)) +
    geom_ribbon(aes(ymin = cumulative_lower_bound, ymax = cumulative_upper_bound, color = NULL), alpha = 0.3) +
    labs(x = "Stem Segment, cm", y = "Cumulative Fraction of Stem Mass", color = "Nitrogen (kg/Ha)", fill = "Nitrogen (kg/Ha)") +
    scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))

# Save plot
ggsave(plot = p4, filename = "visuals/stem_cumulative_mass_fraction_vs_segment_nrate_factor.png",
       height = 8, width = 8, units = "in")
ggsave(plot = p4, filename = "visuals/stem_cumulative_mass_fraction_vs_segment_nrate_factor.tiff",
       height = 8, width = 8, units = "in")

# Even though there is evidence against the null hypothesis that the relative
# mass distribution does not change for different nitrogen rates,
# the difference is still too small to practically use the model.
# Therefore, we'll keep the same model structure,
# but will assume a mean nrate of 100 kg/Ha

m2_rel <- lme(segment_mass_fraction ~ segment + nrate,
          data = serf_segment_data,
          random = ~ 1 | block)

# Save model
saveRDS(m2_rel, file = "data/derived/mxg_stem_model.rds")

# Plot
new_data <- expand_grid(segment = seq(min(serf_segment_data$segment),
                                      max(serf_segment_data$segment),
                                      by = 4), 
                        nrate = 112)

prds <- predict_lme(m2_rel, interval = "conf", newdata = new_data)

prds <- new_data %>% bind_cols(prds)

p5 <- ggplot() +
    geom_point(data = serf_segment_data, aes(x = segment, y = segment_mass_fraction)) +
    geom_line(data = prds, aes(x = segment, y = Estimate)) +
    geom_ribbon(data = prds, aes(x = segment, ymin = Q2.5, ymax = Q97.5), alpha = 0.3) +
    labs(x = "Stem Segment, cm", y = "Fraction of Stem Mass") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))

ggsave(plot = p5, filename = "visuals/stem_mass_fraction_vs_segment.png",
       height = 8, width = 8, units = "in")
ggsave(plot = p5, filename = "visuals/stem_mass_fraction_vs_segment.tiff",
       height = 8, width = 8, units = "in")

# Plot cumulative fractions
serf_segment_data_cumulative <- serf_segment_data %>%
    group_by(plot) %>%
    mutate(cumulative_segment_mass_fraction = cumsum(segment_mass_fraction))

prds_cumulative <- prds %>%
    mutate(cumulative_segment_mass = cumsum(Estimate),
           cumulative_lower_bound = cumsum(Q2.5),
           cumulative_upper_bound = cumsum(Q97.5))

p6 <- ggplot() +
    geom_point(data = serf_segment_data_cumulative, aes(x = segment, y = cumulative_segment_mass_fraction)) +
    geom_line(data = prds_cumulative, aes(x = segment, y = cumulative_segment_mass)) +
    geom_ribbon(data = prds_cumulative, aes(x = segment, ymin = cumulative_lower_bound, ymax = cumulative_upper_bound), alpha = 0.3) +
    labs(x = "Stem Segment (cm)", y = "Cumulative Fraction of Stem Mass") +
    scale_y_continuous(breaks = seq(0, ceiling(max(serf_segment_data_cumulative$cumulative_segment_mass_fraction)), by = 0.05)) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))

ggsave(plot = p6, filename = "visuals/stem_cumulative_mass_fraction_vs_segment.png",
       height = 8, width = 8, units = "in")
ggsave(plot = p6, filename = "visuals/stem_cumulative_mass_fraction_vs_segment.tiff",
         height = 8, width = 8, units = "in")

# tangent_df <- tibble(segment = seq(4, 44, by = 4)) %>%
#     mutate(m = 0.0059 - (4*10^(-5))*segment) %>%
#     filter(segment == 32) %>%
#     mutate(b = 0.175 - (m*32)) %>%
#     select(m, b)

# p7 <- ggplot() +
#     geom_point(data = serf_segment_data_cumulative, aes(x = segment, y = cumulative_segment_mass_fraction)) +
#     geom_line(data = prds_cumulative, aes(x = segment, y = cumulative_segment_mass)) +
#     geom_ribbon(data = prds_cumulative, aes(x = segment, ymin = cumulative_lower_bound, ymax = cumulative_upper_bound), alpha = 0.3) +
#     geom_abline(data = tangent_df, aes(slope = m, intercept = b), color = "red") +
#     labs(x = "Stem Segment (cm)", y = "Cumulative Fraction of Stem Mass") +
#     scale_y_continuous(breaks = seq(0, ceiling(max(serf_segment_data_cumulative$cumulative_segment_mass_fraction)), by = 0.05)) +
#     theme_bw() +
#     theme(axis.title = element_text(face = "bold"),
#           axis.text = element_text(face = "bold"),
#           legend.text = element_text(face = "bold"),
#           legend.title = element_text(face = "bold"))

# ggsave(plot = p7, filename = "visuals/cutting_height/stem_cumulative_mass_fraction_vs_segment_tangent.png",
#        height = 8, width = 8, units = "in")

