# calc_stem_mass_stats.R
# By: Bryan Petersen
# Date: 2024-08-23
# Purpose: Calculate various statistics for the stem mass data
# Input: data/internal/mxg/serf_segment_data.csv

# Load libraries
library(tidyverse)

# Read data
data_tbl <- read_csv(file = "data/internal/mxg/serf_segment_data.csv")

# Calculate the average stem mass and 95% confidence interval for each nrate
stem_mass_stats_tbl <- data_tbl %>%
    distinct(nrate, average_total_stem_mass, stem_count) %>%
    mutate(yield = average_total_stem_mass * stem_count / 100) %>%
    group_by(nrate) %>%
    summarise(mean_stem_mass = mean(average_total_stem_mass),
              ci_stem_mass = qt(0.975, n() - 1) * sd(average_total_stem_mass) / sqrt(n()),
              mean_yield = mean(yield),
              ci_yield = qt(0.975, n() - 1) * sd(yield) / sqrt(n())) %>% 
    ungroup() %>% 
    pivot_longer(cols = c(mean_stem_mass, ci_stem_mass, mean_yield, ci_yield),
                 names_to = "statistic",
                 values_to = "value")

stem_mass_stats_tbl
