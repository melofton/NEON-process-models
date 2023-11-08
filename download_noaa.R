
library(tsibble)
library(tidyverse)
library(neon4cast)
library(lubridate)
library(arrow)


options(dplyr.summarise.inform = FALSE)

# Target data
targets <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)

sites <- unique(targets$site_id)

site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(aquatics == 1 & field_site_id %in% c("BARC","CRAM","LIRO","PRLA","PRPO","SUGG","TOOK"))

# Script to download NOAA data
# Loop through each site to download the stage 2 and stage 3 data
# then combine into individual df
noaa_past_mean <- NULL
noaa_future_mean <- NULL

# make connections to noaa data

# Past stacked weather
df_past <- neon4cast::noaa_stage3()
# Forecasts


df_future <- neon4cast::noaa_stage2(start_date = curr_reference_datetime)


for (i in 1:length(site_data$field_site_id)) {
  test_site <- site_data$field_site_id[i]
  
  # Only need the air temperature from the test_site
  noaa_past <- df_past |> 
    dplyr::filter(site_id %in% test_site,
                  datetime >= ymd('2017-01-01'),
                  variable %in% c("air_temperature", "surface_downwelling_shortwave_flux_in_air")) |>  
    dplyr::collect()
  message(site_data$field_site_id[i], ' stage 3 data downloaded')
  

  # aggregate the past to mean daily values
  noaa_past_agg <- noaa_past |> 
    mutate(datetime = as_date(datetime)) |> 
    group_by(datetime, site_id, variable) |> 
    summarize(daily_mean = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
    rename(datetime = datetime) |> 
    # convert air temp to C
    mutate(daily_mean = ifelse(variable == "air_temperature", daily_mean - 273.15, daily_mean)) |>
    pivot_wider(names_from = variable, values_from = daily_mean)
  
  # New forecast only available at 5am UTC the next day
  forecast_date <- curr_reference_datetime 
  noaa_date <- forecast_date
  max_horizon <- curr_reference_datetime + 30
  
  noaa_future <- df_future |> 
    dplyr::filter(reference_datetime == noaa_date,
                  datetime >= forecast_date,
                  site_id %in% test_site,
                  variable %in% c("air_temperature", "surface_downwelling_shortwave_flux_in_air")) |> 
    dplyr::collect()
  
  # Aggregate for each ensemble for future
  noaa_future_agg <- noaa_future |> 
    mutate(datetime = as_date(datetime)) |> 
    group_by(datetime, site_id, parameter, variable) |> 
    summarize(daily_mean = mean(prediction)) |> 
    mutate(daily_mean = ifelse(variable == "air_temperature", daily_mean - 273.15, daily_mean)) |> 
    pivot_wider(names_from = variable, values_from = daily_mean) |>
    select(datetime, site_id, air_temperature, surface_downwelling_shortwave_flux_in_air, parameter)
  message(site_data$field_site_id[i], ' stage 2 data downloaded')
  
  noaa_past_mean <- bind_rows(noaa_past_mean, noaa_past_agg) 
  noaa_future_mean <- bind_rows(noaa_future_mean, noaa_future_agg) %>%
    filter(datetime <= max_horizon)
  
  
}
 