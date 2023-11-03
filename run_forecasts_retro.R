library(tidyverse)
library(tsibble)
library(aws.s3)

# check for any missing forecasts
message("==== Checking for missed climatology forecasts ====")
challenge_model_names <- c('procCTMIMonod','procBlanchardMonod','procBlanchardSteele',
                           'procCTMISteele','procEppleyNorbergMonod','procEppleyNorbergSteele',
                           'procHinshelwoodMonod','procHinshelwoodSteele')


# Dates of forecasts 
today <- paste(Sys.Date() - days(2), '00:00:00')
this_year <- data.frame(date = as.character(paste0(seq.Date(as_date('2023-01-01'), to = as_date(today), by = 'day'), ' 00:00:00')),
                        exists_procCTMIMonod = NA,
                        exists_procBlanchardMonod = NA,
                        exists_procBlanchardSteele = NA,
                        exists_procCTMISteele = NA,
                        exists_procEppleyNorbergMonod = NA,
                        exists_procEppleyNorbergSteele = NA,
                        exists_procHinshelwoodMonod = NA,
                        exists_procHinshelwoodSteele = NA) 

# what forecasts have already been submitted?
challenge_s3_region <- "data"
challenge_s3_endpoint <- "ecoforecast.org"

# is that file present in the bucket?
for (i in 1:nrow(this_year)) {
  for(j in 1:length(challenge_model_names)){
  forecast_file <- paste0('aquatics-', as_date(this_year$date[i]), '-', challenge_model_names[j], '.csv.gz')
  
  this_year[i,j+1] <- suppressMessages(aws.s3::object_exists(object = file.path("raw", 'aquatics', forecast_file),
                                                                bucket = "neon4cast-forecasts",
                                                                region = challenge_s3_region,
                                                                base_url = challenge_s3_endpoint))
  }
  }

# which dates do you need to generate forecasts for?
missed_dates <- this_year |> 
  filter(if_any(starts_with("exists"), ~ . == F)) |> 
  pull(date) |> 
  as_date()

noaa_missing_dates <- as_date("2023-01-07","2023-01-20","2023-05-23")

if (length(missed_dates) != 0) {
  for (i in 1:length(missed_dates)) {
    curr_reference_datetime <- missed_dates[i]
    
    if(curr_reference_datetime %in% noaa_missing_dates) next
    
    message(paste("creating forecasts for",print(curr_reference_datetime)))
    
    # download the noaa once then apply the forecasts
    source('download_noaa.R')
    message('NOAA downloads complete!')
    
    message('checking for NAs in NOAA data')
    if(any(is.na(noaa_past_mean$air_temperature))) next
    if(any(is.na(noaa_past_mean$surface_downwelling_shortwave_flux_in_air))) next
    if(any(is.na(noaa_future_mean$air_temperature))) next
    if(any(is.na(noaa_future_mean$surface_downwelling_shortwave_flux_in_air))) next
    
    # Script to run forecasts
    source("ignore_sigpipes.R")
    source("./Models/processModelFunctions.R")
    source('./Models/procCTMIMonod.R')
    message('procCTMIMonod model submitted')
    source('./Models/procCTMISteele.R')
    message('procCTMISteele model submitted')
    source('./Models/procBlanchardSteele.R')
    message('procBlanchardSteele model submitted')
    source('./Models/procBlanchardMonod.R')
    message('procBlanchardMonod model submitted')
    source('./Models/procHinshelwoodMonod.R')
    message('procHinshelwoodMonod model submitted')
    source('./Models/procHinshelwoodSteele.R')
    message('procHinshelwoodSteele model submitted')
    source('./Models/procEppleyNorbergSteele.R')
    message('procEppleyNorbergSteele model submitted')
    source('./Models/procEppleyNorbergMonod.R')
    message('procEppleyNorbergMonod model submitted')
    
  }
} else {
  message('no missed forecasts')  
}


