#!/usr/bin/env Rscript

# Load libraries
library(tidyverse)
library(readxl)

# Create directory if it doesn't already exist
if (!file.exists("data/cutting_height")) {
    dir.create("data/cutting_height")
}

# Read datasheet and convert to long dataframe
mxg_segment_df <- read_excel("manual_data/cutting_height/observed/datasheet.xlsx") %>% 
    pivot_longer(cols = ends_with("stem_segments"), 
                 names_to = "segment",
                 names_transform = list(segment = readr::parse_number),
                 values_to = "segment_mass") %>%
    mutate(segment_mass = segment_mass/4,
           average_total_stem_mass = `4 stem stem dry biomass (g)`/4,
           nrate = as.factor(Nrate),
           segment_mass_fraction = segment_mass/average_total_stem_mass) %>% 
    select(Block, Plot, nrate, segment, segment_mass, average_total_stem_mass, segment_mass_fraction, `Stem Count (stems/m^2)`) %>% 
    rename("block" = "Block", "plot" = "Plot", "stem_count" = "Stem Count (stems/m^2)")

# Write out to a csv
write_csv(mxg_segment_df, file = "data/cutting_height/serf_segment_data.csv")

