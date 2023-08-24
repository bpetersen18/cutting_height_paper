#!/usr/bin/env Rscript
# plot_cutting_height_revenue.R
# By: Bryan Petersen
# Date: 2023-07-25

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
yield_tbl <- bind_cols(cutting_height_df, cutting_height_preds) %>%
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
    # Assume a yield of 8 ton ac-1
    mutate(yield = 8) %>%
    # Calculate the yield left in the field
    mutate(yield_left_esti = yield * estimate, 
           yield_left_lower = yield * lower,
           yield_left_upper = yield * upper) %>%
    # Calculate the harvested yield
    mutate(yield_harvested_esti = yield - yield_left_esti,
           yield_harvested_lower = yield - yield_left_lower,
           yield_harvested_upper = yield - yield_left_upper) %>% 
    # Pivot the data frame to long format
    pivot_longer(cols = c(yield_left_esti, yield_left_lower, yield_left_upper,
                          yield_harvested_esti, yield_harvested_lower, yield_harvested_upper),
                 names_to = "variable", values_to = "value") %>% 
    # Mutate the variable column
    mutate(yield_type = str_extract(variable, "yield_(left|harvested)"), 
           estimate_type = str_extract(variable, "(esti|lower|upper)")) %>% 
    # Select columns
    select(cutting_height, yield_type, estimate_type, value) %>% 
    # Pivot the data frame to wide format
    pivot_wider(names_from = estimate_type, values_from = value)

# Plot
p1 <- yield_tbl %>% 
       ggplot() +
       geom_line(aes(x = cutting_height, y = esti, color = yield_type)) +
       geom_ribbon(aes(x = cutting_height, ymin = lower, ymax = upper, fill = yield_type, color = NULL), alpha = 0.2) +
       labs(x = "Cutting height, cm", y = bquote("Biomass, " ~ton%.%ac^-1), fill = NULL, color = NULL) +
       scale_fill_manual(labels = c("Harvested", "Left in field"), values = c("#E69F00", "#56B4E9")) +
       scale_color_manual(labels = c("Harvested", "Left in field"), values = c("#E69F00", "#56B4E9")) +
       theme_bw()

# Save plot
ggsave("visuals/cutting_height_yield_effect.png", p1, width = 8, height = 8, units = "in", dpi = 300)
ggsave("visuals/cutting_height_yield_effect.tiff", p1, width = 8, height = 8, units = "in", dpi = 300)

yield_delta_tbl <- yield_tbl %>% 
       # Pivot the data frame to long format
       pivot_longer(cols = c(esti, lower, upper),
                    names_to = "estimate_type", values_to = "value") %>% 
       # Pivot the data frame to wide format
       pivot_wider(names_from = yield_type, values_from = value) %>% 
       # Create a standard yield left in field and harvested yield columns
       mutate(yield_left_standard = pull(yield_tbl[61,3]), yield_harvested_standard = pull(yield_tbl[62,3])) %>% 
       # Calculate the difference bewteen the standard yield left and harvested
       mutate(yield_left_delta = yield_left - yield_left_standard,
              yield_harvested_delta = yield_harvested - yield_harvested_standard) %>% 
       # Select columns
       select(cutting_height, estimate_type, yield_left_delta, yield_harvested_delta) %>% 
       # Pivot the data frame to long format
       pivot_longer(cols = c(yield_left_delta, yield_harvested_delta),
                    names_to = "yield_type", values_to = "value") %>% 
       # Pivot the data frame to wide format
       pivot_wider(names_from = estimate_type, values_from = value)

# Plot
p2 <- yield_delta_tbl %>% 
       ggplot() +
       geom_line(aes(x = cutting_height, y = esti, color = yield_type)) +
       geom_ribbon(aes(x = cutting_height, ymin = lower, ymax = upper, fill = yield_type, color = NULL), alpha = 0.2) +
       labs(x = "Cutting height, cm", y = bquote("Biomass Change, " ~Mg%.%ha^-1), fill = NULL, color = NULL) +
       scale_fill_manual(labels = c("Harvested", "Left in field"), values = c("#E69F00", "#56B4E9")) +
       scale_color_manual(labels = c("Harvested", "Left in field"), values = c("#E69F00", "#56B4E9")) +
       theme_bw()

# Save plot
ggsave("visuals/cutting_height_yield_effect_delta.png", p2, width = 6, height = 4, units = "in", dpi = 300)
ggsave("visuals/cutting_height_yield_effect_delta.tiff", p2, width = 6, height = 4, units = "in", dpi = 300)

revenue_delta_tbl <- yield_delta_tbl %>% 
       # Pivot the data frame to long format
       pivot_longer(cols = c(esti, lower, upper),
                    names_to = "estimate_type", values_to = "value") %>%
       # Pivot the data frame to wide format
       pivot_wider(names_from = yield_type, values_from = value) %>% 
       # Convert biomass to carbon (Carbon fraction from Meehan et al. 2013)
       mutate(carbon_left_delta = yield_left_delta * 0.49) %>% 
       # Calculate the revenue from the carbon left in the field assuming $44 Mg-1 (https://www.science.org/content/article/farmers-paid-millions-trap-carbon-soils-will-it-actually-help-planet)
       mutate(carbon_revenue = carbon_left_delta * 44) %>% 
       # Calculate the revenue from the biomass harvested assuming $100 Mg-1
       mutate(biomass_revenue = yield_harvested_delta * 100) %>% 
       # Calculate the total revenue
       mutate(total_revenue = carbon_revenue + biomass_revenue)

# Plot
p3 <- revenue_delta_tbl %>% 
       # Select columns
       select(cutting_height, estimate_type, total_revenue, carbon_revenue, biomass_revenue) %>% 
       # Pivot the data frame to long format
       pivot_longer(cols = c(total_revenue, carbon_revenue, biomass_revenue),
                    names_to = "revenue_type", values_to = "value") %>%
       # Pivot the data frame to wide format
       pivot_wider(names_from = estimate_type, values_from = value) %>%
       ggplot() +
       geom_line(aes(x = cutting_height, y = esti, color = revenue_type)) +
       geom_ribbon(aes(x = cutting_height, ymin = lower, ymax = upper, fill = revenue_type, color = NULL), alpha = 0.2) +
       labs(x = "Cutting height, cm", y = bquote("Revenue Change, " ~USD%.%ha^-1), fill = NULL, color = NULL) +
       scale_fill_manual(labels = c("Total Revenue", "Carbon Revenue", "Biomass Revenue"), values = c("black", "#56B4E9", "#E69F00")) +
       scale_color_manual(labels = c("Total Revenue", "Carbon Revenue", "Biomass Revenue"), values = c("black", "#56B4E9", "#E69F00")) +
       theme_bw() 

# Save plot
ggsave("visuals/cutting_height_revenue_effect_delta.png", p3, width = 6, height = 4, units = "in", dpi = 300)

