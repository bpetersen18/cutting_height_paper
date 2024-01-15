# plot_weather_data.R
# By: Bryan Petersen
# Date: 2024-01-13
# Purpose: Plot the weather data for the 2020 growing season at the Southeast Research Farm (SERF) against the climatology
# Input: data/external/weather/columbus_junct_data.txt
#        data/external/weather/serf_sm_mesonet.txt
# Output: data/internal/weather/serf_weather_climo_stats.csv
#         visuals/weather/gdd_2020.png
#         visuals/weather/gdd_2020.tiff
#         visuals/weather/precip_2020.png
#         visuals/weather/precip_2020.tiff

# Load libraries
library(tidyverse)
library(lubridate)

# Create the "visuals/weather" directory if it doesn't exist
dir.create("visuals/weather", showWarnings = FALSE, recursive = TRUE)

# Create the "data/internal/weather" directory if it doesn't exist
dir.create("data/internal/weather", showWarnings = FALSE, recursive = TRUE)

# Read in COOP data
coop_data <- read_csv("data/external/weather/columbus_junct_data.txt") %>% 
    # Calculate average daily temperature
    mutate(avg_temp = (highc + lowc) / 2) %>% 
    # Get the year and day of year
    mutate(year = year(day),
           doy = yday(day)) %>%
    # Calculate the growing degree days
    mutate(base_temp = if_else((highc + lowc)/2 < 6, (highc + lowc)/2, 6),
           gdd = ((highc + lowc)/2) - base_temp) %>%
    # Select only the column we need
    select(day, year, doy, gdd, precipmm)

full_years <- coop_data %>% 
    # Count the number of days in each year
    count(year) %>% 
    # Filter to only full years
    filter(n >= 365) %>%
    pull(year)

full_coop_data <- coop_data %>% 
    # Filter to only full years
    filter(year %in% full_years) %>% 
    # Remove leap days
    filter(doy != 366)

annual_climo_stats <- full_coop_data %>%
    # Group by year
    group_by(year) %>% 
    # Sum the growing degree days and precipitation
    summarise(gdd = sum(gdd, na.rm = TRUE),
              precip = sum(precipmm, na.rm = TRUE)) %>% 
    # Calculate the mean and standard deviation of growing degree days and precipitation
    summarise(mean_gdd = mean(gdd, na.rm = TRUE),
              sd_gdd = sd(gdd, na.rm = TRUE),
              mean_precip = mean(precip, na.rm = TRUE),
              sd_precip = sd(precip, na.rm = TRUE)) %>% 
    mutate(month = NA_integer_)

monthly_climo_stats <- full_coop_data %>% 
    # Get month
    mutate(month = month(day)) %>%
    # Group by year and month
    group_by(month, year) %>%
    # Sum the growing degree days and precipitation
    summarise(gdd = sum(gdd, na.rm = TRUE),
              precip = sum(precipmm, na.rm = TRUE)) %>% 
    # Calculate the mean and standard deviation of growing degree days and precipitation
    summarise(mean_gdd = mean(gdd, na.rm = TRUE),
              sd_gdd = sd(gdd, na.rm = TRUE),
              mean_precip = mean(precip, na.rm = TRUE),
              sd_precip = sd(precip, na.rm = TRUE))

climo_stats <- rbind(annual_climo_stats, monthly_climo_stats) %>%
    # Convert month to factor
    mutate(month = as.factor(month)) %>%
    mutate(month = if_else(is.na(month), "Annual", month))

daily_precip_climo_data <- full_coop_data %>% 
    # Group by year
    group_by(year) %>%
    # Cumulative sum of precip
    mutate(cumulative_precip = cumsum(precipmm)) %>% 
    # Ungroup
    ungroup() %>%
    # Remove precipmm column
    select(!precipmm) %>% 
    # Group by doy
    group_by(doy) %>%
    # Calculate the mean and 95th and 5th percentiles of cumulative precip
    summarise(mean_precip = mean(cumulative_precip, na.rm = TRUE),
              upper_precip = quantile(cumulative_precip, 0.95, na.rm = TRUE),
              lower_precip = quantile(cumulative_precip, 0.05, na.rm = TRUE))

daily_gdd_climo_data <- full_coop_data %>% 
    # Group by year
    group_by(year) %>%
    # Cumulative sum of gdd
    mutate(cumulative_gdd = cumsum(gdd)) %>%
    # Ungroup
    ungroup() %>%
    # Remove gdd column
    select(!gdd) %>%
    # Group by doy
    group_by(doy) %>%
    # Calculate the mean and 95th and 5th percentiles of cumulative gdd
    summarise(mean_gdd = mean(cumulative_gdd, na.rm = TRUE),
              upper_gdd = quantile(cumulative_gdd, 0.95, na.rm = TRUE),
              lower_gdd = quantile(cumulative_gdd, 0.05, na.rm = TRUE))

# Read in the daily weather data from the ISU-SM mesonet
serf_weather <- read_csv("data/external/weather/serf_sm_mesonet.txt") %>% 
    # Convert the temperature to degrees C
    mutate(highc = (high - 32) * 5 / 9,
           lowc = (low - 32) * 5 / 9) %>%
    # Set the base temperature
    mutate(base_temp = if_else((highc + lowc)/2 < 6, (highc + lowc)/2, 6)) %>%
    # Calculate growing degree days
    mutate(gdd = ((highc + lowc)/2) - base_temp) %>%
    # Convert precip to mm
    mutate(precip = precip * 25.4) %>%
    # Set invalid numbers to 0 for gdd
    mutate(gdd = if_else(is.na(gdd), 0, gdd)) %>%
    # Calculate the cumulative precipitation and growing degree days
    mutate(cumulative_precip = cumsum(precip),
           cumulative_gdd = cumsum(gdd)) %>%
    # Get the year and day of year
    mutate(year = year(valid),
           doy = yday(valid)) %>%
    # Select only the columns we need
    select(valid, year, doy, precip, gdd, cumulative_gdd, cumulative_precip)

monthly_serf_weather_stats <- serf_weather %>% 
    # Get month
    mutate(month = month(valid)) %>%
    # Group by month
    group_by(month) %>%
    # Sum the growing degree days and precipitation
    summarise(gdd = sum(gdd, na.rm = TRUE),
              precip = sum(precip, na.rm = TRUE))

annual_serf_weather_stats <- serf_weather %>% 
    # Group by year
    group_by(year) %>%
    # Sum the growing degree days and precipitation
    summarise(gdd = sum(gdd, na.rm = TRUE),
              precip = sum(precip, na.rm = TRUE)) %>% 
    # Rename the year column to month
    rename(month = year) %>%
    mutate(month = NA_integer_)

serf_weather_stats <- rbind(annual_serf_weather_stats, monthly_serf_weather_stats) %>%
    # Convert month to factor
    mutate(month = as.factor(month)) %>%
    mutate(month = if_else(is.na(month), "Annual", month)) %>% 
    # Rename the gdd and precip columns
    rename(gdd_2020 = gdd,
           precip_2020 = precip)

# Join the weather and climate stats
serf_weather_climo_stats <- serf_weather_stats %>% 
    left_join(climo_stats, by = "month") %>%
    # Rename month to time period
    rename(time_period = month) %>%
    # Change from month number to month name
    mutate(time_period = factor(time_period, levels = c("Annual", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                                labels = c("Annual", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>% 
    select(time_period, gdd_2020, mean_gdd, sd_gdd, precip_2020, mean_precip, sd_precip)

# Write the serf weather climo stats to a csv
write_csv(serf_weather_climo_stats, "data/internal/weather/serf_weather_climo_stats.csv")
    
# Join the climotology data to the weather data
serf_weather_climo <- serf_weather %>% 
    left_join(daily_gdd_climo_data, by = "doy") %>% 
    left_join(daily_precip_climo_data, by = "doy")

# Plot the 2020 temperature data with the climotology
p1 <- ggplot(serf_weather_climo, aes(x = valid)) +
    geom_ribbon(aes(ymin = lower_gdd, ymax = upper_gdd, fill = "middle 90% quantile"), alpha = 0.5) +
    geom_line(aes(y = mean_gdd, color = "climatology")) +
    geom_line(aes(y = cumulative_gdd, color = "2020")) +
    labs(x = NULL,
         y = expression(paste("Average Growing Degree Days, ", degree, "C day")),
         color = NULL,
         fill = NULL) +
    scale_color_manual(values = c("2020" = "red", "climatology" = "black")) +
    scale_fill_manual(values = c("middle 90% quantile" = "grey")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
    theme_bw() +
    theme(legend.position = c(0.2, 0.8))

# Save the plot
ggsave("visuals/weather/gdd_2020.png", p1, width = 6, height = 4, dpi = 300)
ggsave("visuals/weather/gdd_2020.tiff", p1, width = 6, height = 4, dpi = 300)


# Plot the 2020 precipitation data with the climotology
p2 <- ggplot(serf_weather_climo, aes(x = valid)) +
    geom_ribbon(aes(ymin = lower_precip, ymax = upper_precip, fill = "middle 90% quantile"), alpha = 0.5) +
    geom_line(aes(y = mean_precip, color = "climatology")) +
    geom_line(aes(y = cumulative_precip, color = "2020")) +
    labs(x = NULL,
         y = "Cumulative Precipitation, mm",
         color = NULL,
         fill = NULL) +
    scale_color_manual(values = c("2020" = "red", "climatology" = "black")) +
    scale_fill_manual(values = c("middle 90% quantile" = "grey")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
    theme_bw() +
    theme(legend.position = c(0.2, 0.8))

# Save the plot
ggsave("visuals/weather/precip_2020.png", p2, width = 6, height = 4, dpi = 300)
ggsave("visuals/weather/precip_2020.tiff", p2, width = 6, height = 4, dpi = 300)




