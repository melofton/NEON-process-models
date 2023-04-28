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


j=4

fit_data <- targets %>%
    filter(site_id == forecast_starts$site_id[j] & complete.cases(.)) %>%
    filter(chla <= (mean(chla, na.rm = TRUE) + 3*sd(chla, na.rm = TRUE))) 

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
  
  chla = fit_data$chla
  wtemp = fit_data$air_temperature
  swr = fit_data$surface_downwelling_shortwave_flux_in_air
  
  par <- c(1, 1, 1, 1, 250, 0.85, 0.85)

  fit2 <- optim(par = par, fn = rmse, method = "Nelder-Mead", chla = chla,
                wtemp = wtemp, swr = swr, hessian = FALSE, control=list(parscale=c(par)))
  
  fit2$par
  pred_chla = proc_model(par = par, wtemp, chla, swr)
  plot(fit_data$datetime, chla, ylim = c(0, max(chla)))
  lines(fit_data$datetime, pred_chla, col = "red")
  rmse(par = fit2$par, chla, wtemp, swr)
  
# parms <- data.frame(site_id = sites,
#                     A1 = rep(NA, length(sites)),
#                     E1 = rep(NA, length(sites)),
#                     A2 = rep(NA, length(sites)),
#                     E2 = rep(NA, length(sites)),
#                     I_K = rep(NA, length(sites)),
#                     R_growth = rep(NA, length(sites)),
#                     R_resp = rep(NA, length(sites)),
                    # RMSE = rep(NA, length(sites)))
parms <- read.csv("./Models/procHinshelwoodMonodParameters.csv")
parms[j,c(2:9)] <- c(fit2$par,rmse(par = fit2$par, chla, wtemp, swr))
write.csv(parms, "./Models/procHinshelwoodMonodParameters.csv", row.names = FALSE)

p1 <- read.csv("./Models/procCTMIMonodParameters.csv")
p2 <- read.csv("./Models/procCTMISteeleParameters.csv")
p3 <- read.csv("./Models/procBlanchardSteeleParameters.csv")
p4 <- read.csv("./Models/procBlanchardMonodParameters.csv")

