# model_mxg_stem_linear_density.R
# By: Bryan Petersen
# Date: 2024-01-13
# Purpose: Model the stem linear density in miscanthus using the data from SERF
# Input: data/internal/mxg/serf_segment_data.csv
# Output: data/internal/mxg/stem_model.rds
#         visuals/mxg_stem_model/stem_count_boxplot.tiff
#         data/internal/mxg/segment_linear_density_stats.csv
#         visuals/mxg_stem_model/stem_linear_density_vs_nrate_boxplot.tiff
#         visuals/mxg_stem_model/stem_linear_density_vs_nrate_qqplot.tiff
#         data/internal/mxg/segment_rel_linear_density_stats.csv
#         visuals/mxg_stem_model/stem_rel_linear_density_vs_nrate_boxplot.tiff
#         visuals/mxg_stem_model/stem_rel_linear_density_vs_nrate_qqplot.tiff
#         visuals/mxg_stem_model/stem_linear_density_vs_segment_nrate_factor.tiff
#         visuals/mxg_stem_model/stem_cumulative_mass_vs_segment_nrate_factor.tiff
#         visuals/mxg_stem_model/stem_rel_linear_density_vs_segment_nrate_factor.tiff
#         visuals/mxg_stem_model/stem_cumulative_mass_percent_vs_segment_nrate_factor.tiff
#         visuals/mxg_stem_model/stem_rel_linear_density_vs_segment.tiff
#         visuals/mxg_stem_model/stem_cumulative_mass_percent_vs_segment.tiff
#         data/internal/mxg/final_linear_regression_inferences.csv
#         data/internal/mxg/model_params.csv

# Install nlraa if not installed in the current library
if (!require("nlraa")){install.packages("nlraa", .libPaths(), repos = "https://mirror.las.iastate.edu/CRAN/")}

# Load libraries
library(tidyverse)
library(rstatix)
library(nlraa)
library(nlme)
library(emmeans)
library(ggpubr)
library(performance)

# Create the "visuals/mxg_stem_model" directory if it doesn't exist
dir.create("visuals/mxg_stem_model", showWarnings = FALSE, recursive = TRUE)

# Read csv file
serf_segment_data <- read_csv(file = "data/internal/mxg/serf_segment_data.csv")  %>% 
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

ggsave(plot = p1, filename = "visuals/mxg_stem_model/stem_count_boxplot.tiff",
       height = 8, width = 8, units = "in")

# Are there outliers
stem_count_outliers_tbl <- stem_count %>% 
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


# Student t-test suggests stem count is not different under the two different nitrogen rates

## ----------------------------------------------------- ##
## Simple segment linear density stats grouped by N rate ##
## ----------------------------------------------------- ##
# Get just segment linear density for each plot and segment location
segment_linear_density_tbl <- serf_segment_data %>%
    mutate(segment_linear_density = segment_mass/4) %>% 
    group_by(segment, nrate_factor, plot) %>% 
    summarise(segment_linear_density = mean(segment_linear_density)) %>% 
    ungroup()

# Calculate summary stats
segment_linear_density_stats <- segment_linear_density_tbl %>% 
    group_by(nrate_factor) %>% 
    get_summary_stats(segment_linear_density, type = "mean_ci")

# Write out summary stats
write_csv(segment_linear_density_stats, file = "data/internal/mxg/segment_linear_density_stats.csv")

# Visualize the data using boxplots
ald_bxp <- ggboxplot(segment_linear_density_tbl, x = "nrate_factor", y = "segment_linear_density", ylab = "Stem Linear Density, g/cm", xlab = "Nitrogen, kg/Ha", add = "jitter")

# Save plot
ggsave(plot = ald_bxp, filename = "visuals/mxg_stem_model/stem_linear_density_vs_nrate_boxplot.tiff", height = 8, width = 8, units = "in")

# Are there outliers
ald_outliers_tbl <- segment_linear_density_tbl %>% 
    group_by(nrate_factor) %>% 
    identify_outliers(segment_linear_density)

# There are no extreme outliers

# Check normality by groups
ald_normality_tbl <- segment_linear_density_tbl %>% 
    group_by(nrate_factor) %>% 
    shapiro_test(segment_linear_density)

# The shapiro test suggests the O N rate data is not normal

# Plot qqplot
ald_qqp <- ggqqplot(segment_linear_density_tbl, x = "segment_linear_density", facet.by = "nrate_factor")

# Save plot
ggsave(plot = ald_qqp, filename = "visuals/mxg_stem_model/stem_linear_density_vs_nrate_qqplot.tiff", height = 8, width = 8, units = "in")

# The qqplot looks okay to assume normality. Therefore, we'll assume normality

# Check the equality of variances
ald_levene_test_results <- segment_linear_density_tbl %>% 
    levene_test(segment_linear_density ~ nrate_factor)

# Levene's test suggests the variances are equal

# Compute t-test
ald_stat_test <- segment_linear_density_tbl %>% 
    t_test(segment_linear_density ~ nrate_factor, var.equal = TRUE) %>% 
    add_significance()

# Student t-test suggests segment linear density is different under the two different nitrogen rates

## -------------------------------------------------------------- ##
## Simple segment relative linear density stats grouped by N rate ##
## -------------------------------------------------------------- ##
# Get just segment mass for each plot and segment location
segment_rel_linear_density_tbl <- serf_segment_data %>%
    mutate(segment_rel_linear_density = segment_mass_fraction/4*100) %>% 
    group_by(segment, nrate_factor, plot) %>% 
    summarise(segment_rel_linear_density = mean(segment_rel_linear_density)) %>% 
    ungroup()

# Calculate summary stats
segment_rel_linear_density_tbl_stats <- segment_rel_linear_density_tbl %>% 
    group_by(nrate_factor) %>% 
    get_summary_stats(segment_rel_linear_density, type = "mean_ci")

# Write out summary stats
write_csv(segment_rel_linear_density_tbl_stats, file = "data/internal/mxg/segment_rel_linear_density_stats.csv")

# Visualize the data using boxplots
rld_bxp <- ggboxplot(segment_rel_linear_density_tbl, x = "nrate_factor", y = "segment_rel_linear_density", ylab = "Relative Stem Linear Density, %/cm", xlab = "Nitrogen, kg/Ha", add = "jitter")

# Save plot
ggsave(plot = rld_bxp, filename = "visuals/mxg_stem_model/stem_rel_linear_density_vs_nrate_boxplot.tiff", height = 8, width = 8, units = "in")

# Are there outliers
rld_outliers_tbl <- segment_rel_linear_density_tbl %>% 
    group_by(nrate_factor) %>% 
    identify_outliers(segment_rel_linear_density)

# There are no extreme outliers

# Check normality by groups
rld_normality_tbl <- segment_rel_linear_density_tbl %>% 
    group_by(nrate_factor) %>% 
    shapiro_test(segment_rel_linear_density)

# The shapiro test suggests the O N rate data is not normal

rld_qqp <- ggqqplot(segment_rel_linear_density_tbl, x = "segment_rel_linear_density", facet.by = "nrate_factor")

# The qqplot looks okay to assume normality. There are two outliers in the 0 N rate data that appear to be driving the non-normality in the Shapiro test. We will assume normality for now. We will remove the outliers and see how it affects the mixed linear model later in the script.

# Save plot
ggsave(plot = rld_qqp, filename = "visuals/mxg_stem_model/stem_rel_linear_density_vs_nrate_qqplot.tiff", height = 8, width = 8, units = "in")

# Check the equality of variances
rld_levene_test_results <- segment_rel_linear_density_tbl %>% 
    levene_test(segment_rel_linear_density ~ nrate_factor)

# Levene's test suggests the variances are equal

# Compute t-test
rld_stat_test <- segment_rel_linear_density_tbl %>% 
    t_test(segment_rel_linear_density ~ nrate_factor, var.equal = TRUE) %>% 
    add_significance()

# Student t-test suggests segment linear density is different under the two different nitrogen rates


## --------------------------------- ##
## Modelling absolute linear density ##
## --------------------------------- ##
# Mixed model of segment stem linear density with segment
# location (4 represents segement from 0 to 4 cm
# and 9 represents from 4 to 8 cm), nitrogen rate (factor), and
# interaction as fixed effects and block as random effects
serf_segment_data <- serf_segment_data %>% 
    mutate(segment_linear_density = segment_mass/4,
           segment_rel_linear_density = segment_mass_fraction/4*100)
m1_abs <- lme(segment_linear_density ~ segment * nrate_factor,
              data = serf_segment_data,
              random = ~ 1 | block)

# Test null hypothesis that there is no interaction between segment and nitrogen rate
anova(m1_abs, type = "sequential")

# Anova indicates there is no evidence against the null hypothesis

# Remove the interaction term from the model
m1_abs <- lme(segment_linear_density ~ segment + nrate_factor,
              data = serf_segment_data,
              random = ~ 1 | block)

# Calculate the adjusted R^2 using performance package
m1_abs_r2 <- r2_nakagawa(m1_abs)

# Get the fix effects table
m1_abs_effects <- summary(m1_abs)$tTable
m1_abs_slope <- signif(m1_abs_effects[2, 1], 2)
m1_abs_intercept <- signif(m1_abs_effects[1, 1], 2)
m1_abs_nrate_effect <- signif(m1_abs_effects[3, 1], 2)

# Plot model with data
abs_prds <- predict_lme(m1_abs, interval = "conf")
abs_data_prds <- serf_segment_data %>% bind_cols(abs_prds)

# Get the 95% confidence interval for the slope and intercepts
m1_abs_ci <- intervals(m1_abs, level = 0.95)

# Tidy up the confidence intervals
model_params_tbl <- m1_abs_ci$fixed %>% 
    as_tibble(rownames = "term") %>% 
    mutate(model = "ald")

p1 <- ggplot(data = abs_data_prds, aes(x = segment, color = nrate_factor, fill = nrate_factor)) +
    geom_point(aes(y = segment_linear_density)) +
    geom_line(aes(y = Estimate)) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, color = NULL), alpha = 0.3) +
    # Add r-squared to the plot and equation
    annotate("text", x = 6.5, y = max(abs_data_prds$segment_linear_density)-0.09, 
             label = bquote("R"^2 == .(round(as.numeric(m1_abs_r2[["R2_conditional"]]), 3))), size = 6) +
    annotate("text", x = 12, y = max(abs_data_prds$segment_linear_density)-0.01,
             label = bquote(y[N[0]] == .(m1_abs_slope) ~ x + ~ .(m1_abs_intercept)), size = 6) +
    annotate("text", x = 12, y = max(abs_data_prds$segment_linear_density)-0.05,
             label = bquote(y[N[224]] == .(m1_abs_slope) ~ x + ~ .(signif(m1_abs_nrate_effect + m1_abs_intercept, 2))), size = 6) +
    labs(x = "Stem Segment, cm", y = bquote(bold("Stem Linear Density, " ~g%.%cm^-1)), color = bquote(bold("Nitrogen, " ~kg%.%ha^-1)), fill = bquote(bold("Nitrogen, " ~kg%.%ha^-1))) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold", size = 16),
          axis.text = element_text(face = "bold", size = 16),
          legend.text = element_text(face = "bold", size = 16),
          legend.title = element_text(face = "bold", size = 16),
          legend.position = c(0.8, 0.8))

ggsave(plot = p1, filename = "visuals/mxg_stem_model/stem_linear_density_vs_segment_nrate_factor.jpg",
       height = 8, width = 8, units = "in")

# Plot cumulative stem mass
segment_data_cumulative <- serf_segment_data %>%
    group_by(plot) %>%
    mutate(cumulative_segment_mass = cumsum(segment_mass)) %>% 
    ungroup()

prds_cumulative <- abs_data_prds %>%
    group_by(plot) %>%
    mutate(cumulative_segment_mass = cumsum(Estimate*4),
           cumulative_lower_bound = cumsum(Q2.5*4),
           cumulative_upper_bound = cumsum(Q97.5*4)) %>% 
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

ggsave(plot = p2, filename = "visuals/mxg_stem_model/stem_cumulative_mass_vs_segment_nrate_factor.tiff",
       height = 8, width = 8, units = "in")


## --------------------------------- ##
## Modelling relative linear density ##
## --------------------------------- ##
# Mixed model of the relative linear density with segment
# location (4 represents segement from 0 to 4 cm
# and 9 represents from 4 to 8 cm), nitrogen rate (factor), and
# interaction as fixed effects and block as random effects
m1_rel  <- lme(segment_rel_linear_density ~ segment * nrate_factor,
           data = serf_segment_data,
           random = ~ 1 | block)

# Test null hypothesis that there is no interaction between segment and nitrogen rate
anova(m1_rel, type = "sequential")

# Anova indicates there is no evidence against the null hypothesis

# Remove the interaction term from the model
m1_rel <- lme(segment_rel_linear_density ~ segment + nrate_factor,
           data = serf_segment_data,
           random = ~ 1 | block)

# Calculate the adjusted R^2 using performance package
m1_rel_r2 <- r2_nakagawa(m1_rel)

# Get the fix effects table
m1_rel_effects <- summary(m1_rel)$tTable
m1_rel_slope <- signif(m1_rel_effects[2, 1], 2)
m1_rel_intercept <- signif(m1_rel_effects[1, 1], 2)
m1_rel_nrate_effect <- signif(m1_rel_effects[3, 1], 2)

# Get the 95% confidence interval for the slope and intercepts
m1_rel_ci <- intervals(m1_rel, level = 0.95)

# Tidy up the confidence intervals
model_params_tbl <- m1_rel_ci$fixed %>% 
    as_tibble(rownames = "term") %>% 
    mutate(model = "rld") %>% 
    bind_rows(model_params_tbl)

# Write to file
write_csv(model_params_tbl, file = "data/internal/mxg/model_params.csv")

# Plot model with data
prds <- predict_lme(m1_rel, interval = "conf")
serf_segment_data_prds <- serf_segment_data %>% bind_cols(prds)

p3 <- ggplot(data = serf_segment_data_prds, aes(x = segment, color = nrate_factor, fill = nrate_factor)) +
    geom_point(aes(y = segment_rel_linear_density)) +
    geom_line(aes(y = Estimate)) +
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, color = NULL), alpha = 0.3) +
    # Add r-squared to the plot and equation
    annotate("text", x = 11, y = max(serf_segment_data_prds$segment_rel_linear_density)-0.09, 
             label = bquote(R^2 == .(round(as.numeric(m1_rel_r2[["R2_conditional"]]), 3))), size = 6) +
    annotate("text", x = 15, y = max(serf_segment_data_prds$segment_rel_linear_density)-0.01,
             label = bquote(y[N[0]] ==  .(m1_rel_slope)~ x + ~ .(m1_rel_intercept)), size = 6) +
    annotate("text", x = 15, y = max(serf_segment_data_prds$segment_rel_linear_density)-0.05,
             label = bquote(y[N[224]] == .(m1_rel_slope) ~ x + ~ .(signif(m1_rel_nrate_effect + m1_rel_intercept, 2))), size = 6) +
    labs(x = "Stem Segment, cm", y = bquote(bold("Relative Stem Linear Density, %" ~"\u00B7"~ cm^-1)), color = bquote(bold("Nitrogen, " ~kg%.%ha^-1)), fill = bquote(bold("Nitrogen, " ~kg%.%ha^-1))) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold", size = 16),
          axis.text = element_text(face = "bold", size = 16),
          legend.text = element_text(face = "bold", size = 16),
          legend.title = element_text(face = "bold", size = 16),
          legend.position = c(0.8, 0.8))

# Save plot
ggsave(plot = p3, filename = "visuals/mxg_stem_model/stem_rel_linear_density_vs_segment_nrate_factor.jpg",
       height = 8, width = 8, units = "in")

# Plot cumulative mass percent
p4 <- serf_segment_data_prds %>% 
    group_by(plot) %>% 
    mutate(cumulative_segment_mass_percent_obs = cumsum(segment_rel_linear_density*4),
           cumulative_segment_mass_percent_mod = cumsum(Estimate*4),
           cumulative_lower_bound = cumsum(Q2.5*4),
           cumulative_upper_bound = cumsum(Q97.5*4)) %>% 
    ungroup() %>% 
    ggplot(aes(x = segment, color = nrate_factor, fill = nrate_factor)) +
    geom_point(aes(y = cumulative_segment_mass_percent_obs)) +
    geom_line(aes(y = cumulative_segment_mass_percent_mod)) +
    geom_ribbon(aes(ymin = cumulative_lower_bound, ymax = cumulative_upper_bound, color = NULL), alpha = 0.3) +
    labs(x = "Stem Segment, cm", y = "Cumulative Stem Mass Percent", color = bquote(bold("Nitrogen, " ~kg%.%ha^-1)), fill = bquote(bold("Nitrogen, " ~kg%.%ha^-1))) +
    scale_y_continuous(breaks = seq(0, 30, 5)) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))

# Save plot
ggsave(plot = p4, filename = "visuals/mxg_stem_model/stem_cumulative_mass_percent_vs_segment_nrate_factor.jpg",
       height = 8, width = 8, units = "in")

# Even though there is evidence against the null hypothesis that the relative
# mass distribution does not change for different nitrogen rates,
# the difference is still too small to practically use the model.
# Therefore, we'll keep the same model structure,
# but will assume a mean nrate of 100 kg/Ha

m2_rel <- lme(segment_rel_linear_density ~ segment + nrate,
          data = serf_segment_data, 
          random = ~ 1 | block)

# Save model
saveRDS(m2_rel, file = "data/internal/mxg/stem_model.rds")

# Make predictions for numbers in the paper
new_data <- expand_grid(segment = seq(min(serf_segment_data$segment),
                                      max(serf_segment_data$segment),
                                      by = 1), 
                        nrate = 112)
prds <- predict_lme(m2_rel, interval = "conf", newdata = new_data)

prds_paper <- new_data %>% bind_cols(prds) %>% 
    # Rename columns
    rename(rel_linear_density = Estimate,
           rel_linear_density_lower_bound = Q2.5,
           rel_linear_density_upper_bound = Q97.5) %>%
    mutate(cumulative_stem_mass_percent = if_else(segment == 4, rel_linear_density*4, rel_linear_density)) %>% 
    mutate(cumulative_stem_mass_percent = cumsum(cumulative_stem_mass_percent),
           cumulative_stem_mass_percent_lower_bound = if_else(segment == 4, rel_linear_density_lower_bound*4, rel_linear_density_lower_bound),
           cumulative_stem_mass_percent_lower_bound = cumsum(cumulative_stem_mass_percent_lower_bound),
           cumulative_stem_mass_percent_upper_bound = if_else(segment == 4, rel_linear_density_upper_bound*4, rel_linear_density_upper_bound),
           cumulative_stem_mass_percent_upper_bound = cumsum(cumulative_stem_mass_percent_upper_bound))

# Write out the prds_paper data
write_csv(prds_paper, file = "data/internal/mxg/final_linear_regression_inferences.csv")

# Make predictions for plotting
new_data <- expand_grid(segment = seq(min(serf_segment_data$segment),
                                      max(serf_segment_data$segment),
                                      by = 4), 
                        nrate = 112)

prds <- predict_lme(m2_rel, interval = "conf", newdata = new_data)

prds <- new_data %>% bind_cols(prds)

# Plot the relative linear density vs segment
p5 <- ggplot() +
    geom_point(data = serf_segment_data, aes(x = segment, y = segment_rel_linear_density)) +
    geom_line(data = prds, aes(x = segment, y = Estimate)) +
    geom_ribbon(data = prds, aes(x = segment, ymin = Q2.5, ymax = Q97.5), alpha = 0.3) +
    labs(x = "Stem Segment, cm", y = bquote(bold("Relative Stem Linear Density, %" ~"\u00B7"~ cm^-1))) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))

ggsave(plot = p5, filename = "visuals/mxg_stem_model/stem_rel_linear_density_vs_segment.tiff",
       height = 8, width = 8, units = "in")

# Plot cumulative mass percent
serf_segment_data_cumulative <- serf_segment_data %>%
    group_by(plot) %>%
    mutate(cumulative_segment_mass_percent = cumsum(segment_rel_linear_density*4))

prds_cumulative <- prds %>%
    mutate(cumulative_segment_mass = cumsum(Estimate*4),
           cumulative_lower_bound = cumsum(Q2.5*4),
           cumulative_upper_bound = cumsum(Q97.5*4))

p6 <- ggplot() +
    geom_point(data = serf_segment_data_cumulative, aes(x = segment, y = cumulative_segment_mass_percent)) +
    geom_line(data = prds_cumulative, aes(x = segment, y = cumulative_segment_mass)) +
    geom_ribbon(data = prds_cumulative, aes(x = segment, ymin = cumulative_lower_bound, ymax = cumulative_upper_bound), alpha = 0.3) +
    labs(x = "Stem Segment, cm", y = "Cumulative Stem Mass Percent") +
    scale_y_continuous(breaks = seq(0, ceiling(max(serf_segment_data_cumulative$cumulative_segment_mass_percent)), by = 5)) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.text = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))

ggsave(plot = p6, filename = "visuals/mxg_stem_model/stem_cumulative_mass_percent_vs_segment.jpg",
         height = 8, width = 8, units = "in")