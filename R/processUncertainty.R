library(fable)
library(tsibble)
library(tidyverse)
library(neon4cast)
library(lubridate)
library(arrow)

options(dplyr.summarise.inform = FALSE)

# Target data
targets <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)

site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(aquatics == 1)

sites = c("BARC","CRAM","LIRO","PRLA","PRPO","SUGG","TOOK")

models <- c("CTMIMonod","CTMISteele","BlanchardMonod","BlanchardSteele",
            "HinshelwoodMonod","HinshelwoodSteele","EppleyNorbergMonod",
            "EppleyNorbergSteele")

# Do we need a value from yesterday to start?
forecast_starts <- targets %>%
  na.omit() %>%
  group_by(variable, site_id) %>%
  # Start the day after the most recent non-NA value
  dplyr::summarise(start_date = max(datetime) + lubridate::days(1)) %>% # Date
  dplyr::mutate(h = (Sys.Date() - start_date) + 30) %>% # Horizon value
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

targets <- left_join(targets, noaa_past_mean, by = c("datetime","site_id"))

for(i in 1:length(models)){
  
  fil <- paste0("./Models/proc",models[i],"Parameters.csv")
  parms <- read_csv(fil)
  
  for(j in 1:length(sites)){
    
    
  }
}
