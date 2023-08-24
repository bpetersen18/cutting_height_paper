#!/usr/bin/env Rscript
# plot_cutting_height_carbon.R
# By: Bryan Petersen
# Date: 2023-08-24

# Load libraries
library(tidyverse)
library(nlraa)

# Load libraries
library(tidyverse)
library(nlraa)

# Load model
mxg_model <- readRDS("data/derived/mxg_stem_model.rds")

# Read in "data/raw/cutting_height_obs.csv"
cutting_height_obs <- read_csv("data/raw/cutting_height_obs.csv")

# Create a vector of cutting heights from the minimum to the maximum cutting
# height in the observed data
cutting_heights <- seq(0,
                       max(cutting_height_obs$stem_height),
                       by = 1)

# Create a data frame of cutting heights
cutting_height_df <- data.frame(segment = cutting_heights, nrate = 112)

# Predict the fraction of stem mass for each 4 cm segment
cutting_height_preds <- predict_lme(mxg_model, newdata = cutting_height_df, interval = "conf")

# Bind the predictions to the cutting height data frame
carbon_tbl <- bind_cols(cutting_height_df, cutting_height_preds) %>%
    # Convert to a tibble
    as_tibble() %>%
    # Calculate the fraction of stem mass for each 1 cm segment
    mutate(fraction_stem_mass = Estimate/4,
           fraction_stem_mass_lower = `Q2.5`/4,
           fraction_stem_mass_upper = `Q97.5`/4) %>%
    # Select columns
    select(segment, fraction_stem_mass, fraction_stem_mass_lower, fraction_stem_mass_upper) %>%
    # Calculate the fraction of stem mass below the cutting height
    mutate(fraction_stem_mass = cumsum(fraction_stem_mass),
           fraction_stem_mass_lower = cumsum(fraction_stem_mass_lower),
           fraction_stem_mass_upper = cumsum(fraction_stem_mass_upper)) %>% 
    # Rename columns
    rename(cutting_height = segment, 
           estimate = fraction_stem_mass, 
           lower = fraction_stem_mass_lower,
           upper = fraction_stem_mass_upper) %>% 
    # Assume a yield of 17.9 Mg ha-1
    mutate(yield = 17.9) %>%
    # Calculate the yield left in the field
    mutate(yield_left_esti = yield * estimate, 
           yield_left_lower = yield * lower,
           yield_left_upper = yield * upper) %>% 
    # Convert biomass to carbon (Carbon fraction from Meehan et al. 2013)
    mutate(carbon_left_esti = yield_left_esti * 0.49,
           carbon_left_lower = yield_left_lower * 0.49,
           carbon_left_upper = yield_left_upper * 0.49) %>% 
    select(cutting_height, carbon_left_esti, carbon_left_lower, carbon_left_upper)

# Plot
p1 <- carbon_tbl %>% 
    ggplot() +
    geom_line(aes(x = cutting_height, y = carbon_left_esti), color = "black") +
    geom_ribbon(aes(x = cutting_height, ymin = carbon_left_lower, ymax = carbon_left_upper), alpha = 0.3) +
    labs(x = "Cutting Height (cm)", y = "Carbon Left in Field (Mg/ha)") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"))

# Save plot
ggsave("visuals/cutting_height_carbon.png", p1, width = 8, height = 8, dpi = 300)
ggsave("visuals/cutting_height_carbon.tiff", p1, width = 8, height = 8, dpi = 300)
