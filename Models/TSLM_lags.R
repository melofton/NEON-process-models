# TSLM model with air temperature

library(fable)
library(tsibble)
library(tidyverse)
library(neon4cast)
library(lubridate)
#library(rMR)
library(arrow)


options(dplyr.summarise.inform = FALSE)

# submission information
team_name <- "fTSLM_lag"

# team_list <- list(list(individualName = list(givenName = "Freya", 
#                                              surName = "Olsson"),
#                        organizationName = "Virginia Tech",
#                        electronicMailAddress = "freyao@vt.edu"))

# Target data
targets <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)

sites <- unique(targets$site_id)

site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(aquatics == 1)

# Merge in past NOAA data into the targets file, matching by date.
# Before building our linear model we need merge in the historical air 
# temperature to match with the historical water temperature

targets <- targets |> 
  select(datetime, site_id, variable, observation) |> 
  filter(variable == 'temperature') |> 
  pivot_wider(names_from = "variable", values_from = "observation") 

targets <- left_join(targets, noaa_past_mean, by = c("datetime","site_id"))


# NOAA weather - combine the past and future 
# sometimes we need to start the forecast in the past
past_weather <- NULL

# Extract the past weather data for the met observations where we don't have 
# temperature observations
for (i in 1:nrow(forecast_starts)) {
  subset_past_weather <- noaa_past_mean %>%
    # only take the past weather that is after the last water temperature observation and 
    # less than what is in the weather forecast
    filter(site_id == forecast_starts$site_id[i]  &
             datetime >= forecast_starts$start_date[i] &
             datetime < min(noaa_future_mean$datetime)) %>% 
    # slice(rep(1:n(), 31)) %>%
    mutate()
  past_weather <- bind_rows(past_weather, subset_past_weather)
}


# create a past "parameter" - just repeats each value 31 times
past_weather <- past_weather %>%
  group_by(site_id) %>%
  slice(rep(1:n(), 31)) %>%
  group_by(site_id, air_temperature) %>%
  mutate(parameter = row_number())

# Combine the past weather with weather forecast
message('creating weather ensembles')
noaa_weather <- bind_rows(past_weather, noaa_future_mean) %>%
  arrange(site_id, )

message('starting TSLM model fitting and forecast generations')
TSLM_fable <- NULL

for (i in 1:length(site_data$field_site_id)) {
  # Loop through each site
  site_use <- site_data$field_site_id[i]
  # Split the NOAA forecast into each ensemble
  noaa_ensembles <- noaa_weather %>%
    filter(site_id == site_use,
           parameter != 31) %>%
    split(., ~ parameter)
  
  # For each ensemble make this into a tsibble that can be used to forecast
  # then when this is supplied as the new_data argument it will run the forecast for each 
  # air-temperature forecast ensemble
  test_scenarios <- lapply(noaa_ensembles, as_tsibble, key = 'site_id', index = 'datetime')
  
  # Fits separate LM with ARIMA errors for each site
  # message('fitting TSLM model')
  targets_use <- targets %>%
    filter(site_id == site_use) %>%
    as_tsibble(key = 'site_id', index = 'datetime') %>%
    # add NA values up to today (index)
    fill_gaps(.end = Sys.Date()) 
  
  if (nrow(targets_use) >= 1) {
    TSLM_model <- targets_use %>%
      model(TSLM(temperature ~ air_temperature + lag(air_temperature)))
    
    # Forecast using the fitted model
    # message('producing ensemble forecast using TSLM model')
    TSLM_fable_site <-  TSLM_model %>%
      generate(new_data = test_scenarios, bootstrap = T, times = 100) %>%
      mutate(variable = 'temperature',
             parameter = as.numeric(.rep) + (100 * (as.numeric(.scenario) - 1))) %>%
      filter(datetime > Sys.Date())
    
    TSLM_fable <- bind_rows(TSLM_fable, TSLM_fable_site)
    message('TSLM forecast for ', site_use, ' complete')
  } else {
    message('no forecast generated for ', site_use, ' no observations present')
  }
  
  
}

# Function to convert to EFI standard
convert.to.efi_standard <- function(df){
  
  df %>% 
    as_tibble() %>%
    dplyr::rename(prediction = .sim) %>%
    dplyr::select(datetime, site_id, prediction, variable, parameter) %>%
    dplyr::mutate(family = "ensemble",
                  model_id = team_name, 
                  reference_datetime = min(datetime) - lubridate::days(1)) %>%
    dplyr::select(any_of(c('datetime', 'reference_datetime', 'site_id', 'family', 
                           'parameter', 'variable', 'prediction', 'model_id')))
}

# Convert to the EFI standard from a fable with distribution
message('converting to EFI standard')
TSLM_EFI <- convert.to.efi_standard(TSLM_fable)  

forecast_file <- paste0('aquatics-',TSLM_EFI$reference_datetime[1], '-', team_name, '.csv.gz')

write_csv(TSLM_EFI, forecast_file)
# Submit forecast!

# Now we can submit the forecast output to the Challenge using 
neon4cast::forecast_output_validator(forecast_file)
neon4cast::submit(forecast_file = forecast_file,
                    ask = F)
