
# download the noaa once then apply the forecasts
source('download_noaa.R')
message('NOAA downloads complete!')

# Script to run forecasts
source('./Models/ARIMA_model.R')
message('ARIMA model submitted')
source('./Models/TSLM_lags.R')
message('TSLM_lagged model submitted')
