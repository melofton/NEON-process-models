library(fable)
library(tsibble)
library(tidyverse)
library(neon4cast)
library(lubridate)
library(arrow)

options(dplyr.summarise.inform = FALSE)

# submission information
team_name <- "procHinshelwoodMonod"

# Target data
targets <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)

site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(aquatics == 1)

sites = c("BARC","CRAM","LIRO","PRLA","PRPO","SUGG","TOOK")

# Do we need a value from yesterday to start?
forecast_starts <- targets %>%
  na.omit() %>%
  group_by(variable, site_id) %>%
  dplyr::filter(datetime <= curr_reference_datetime) %>%
  # Start the day after the most recent non-NA value
  dplyr::summarise(start_date = max(datetime) + lubridate::days(1)) %>% # Date
  dplyr::mutate(h = (curr_reference_datetime - start_date) + 30) %>% # Horizon value
  dplyr::filter(variable == 'chla' & site_id %in% sites) %>%
  dplyr::ungroup()


# Merge in past NOAA data into the targets file, matching by date.
# Before building our linear model we need merge in the historical air 
# temperature to match with the historical water temperature

targets <- targets |> 
  select(datetime, site_id, variable, observation) |> 
  filter(variable == 'chla') |> 
  pivot_wider(names_from = "variable", values_from = "observation") %>%
  filter(!is.na(chla) & site_id %in% sites)

targets <-  left_join(targets, noaa_past_mean, by = c("datetime","site_id"))



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
    # create a past "ensemble" - just repeats each value 31 times
    slice(rep(1:n(), 31))
  past_weather <- bind_rows(past_weather, subset_past_weather)
}


past_weather <- past_weather %>%
  group_by(site_id, air_temperature) %>%
  mutate(parameter = row_number())


# Combine the past weather with weather forecast
message('creating weather ensembles')
noaa_weather <- bind_rows(past_weather, noaa_future_mean) %>%
  arrange(site_id, parameter)

# Split the NOAA forecast into each ensemble (parameter)
noaa_ensembles <- split(noaa_weather, f = noaa_weather$parameter)
# For each ensemble make this into a tsibble that can be used to forecast
  # then when this is supplied as the new_data argument it will run the forecast for each 
  # air-temperature forecast ensemble
test_scenarios <- lapply(noaa_ensembles, as_tsibble, key = 'site_id', index = 'datetime')


message('starting procHinshelwoodMonod forecast generations')

#define model
#build process model
#@states vector of named states
#@par vector of named parameters
source("./Models/processModelFunctions.R")
proc_model <- function(par, wtemp, chla, swr){
  pred_chla = NULL
  pred_chla[1] <- chla[1]
  for(i in 2:length(wtemp)){
    
    fT = hinshelwood(wtemp = wtemp[i],
                   A1 = par[1],
                   E1 = par[2],
                   A2 = par[3],
                   E2 = par[4],
                   R = 8.3145)
    fI = monod(swr = swr[i],
                I_K = par[5])
    fR = 1.08^(wtemp[i] - 20)
    
    growth = pred_chla[i-1] * par[6] * min(fT, fI)
    respiration = pred_chla[i-1] * par[7] * fR
    
    pred_chla[i] = pred_chla[i-1] + growth - respiration 
    
  }
  return(pred_chla)
}

forecast <- tibble(datetime = Date(),
                   site_id = as.character(),
                   prediction = as.numeric(),
                   variable = as.character(),
                   parameter = as.numeric())

parms <- read_csv("./Models/procHinshelwoodMonodParameters.csv")

for(j in 1:nrow(forecast_starts)){
  
  chla = targets %>%
    filter(site_id == forecast_starts$site_id[j] & complete.cases(.)) %>%
    pull(chla) 
  
  par = parms %>%
    filter(site_id == forecast_starts$site_id[j]) %>%
    pivot_longer(A1:R_resp, names_to = "parameter", values_to = "value") %>%
    pull(value)
  
  for(k in 1:length(test_scenarios)){
    
    forecast_data <- test_scenarios[[k]] %>%
      filter(site_id == sites[j])
    
    wtemp = forecast_data$air_temperature
    swr = forecast_data$surface_downwelling_shortwave_flux_in_air
    
    pred_chla <- proc_model(par = par,
                            wtemp = wtemp,
                            chla = chla,
                            swr = swr)
    
    fc <- tibble(datetime = forecast_data$datetime,
                 site_id = forecast_data$site_id,
                 prediction = pred_chla,
                 variable = "chla",
                 parameter = forecast_data$parameter)
    
    forecast <- bind_rows(forecast, fc)
    
  }
  
}

forecast <- forecast %>%
  filter(datetime >= curr_reference_datetime)

message('forecast generated')

# Function to convert to EFI standard
convert.to.efi_standard <- function(df){
  
  df %>% 
    as_tibble() %>%
    dplyr::mutate(family = "ensemble",
                  model_id = team_name,
                  reference_datetime = min(datetime) - lubridate::days(1)) %>%
    dplyr::select(any_of(c('datetime', 'reference_datetime', 'site_id', 'family', 
                           'parameter', 'variable', 'prediction', 'model_id')))
}
message('converting to EFI standard')
procHinshelwoodMonod_EFI <- convert.to.efi_standard(forecast)
  

forecast_file <- paste0('aquatics-', procHinshelwoodMonod_EFI$reference_datetime[1], '-', team_name, '.csv.gz')

write_csv(procHinshelwoodMonod_EFI, forecast_file)
# Submit forecast!

# Now we can submit the forecast output to the Challenge using 
neon4cast::forecast_output_validator(forecast_file)
neon4cast::submit(forecast_file = forecast_file,
                  ask = F, s3_region = 'data', s3_endpoint = 'ecoforecast.org')
