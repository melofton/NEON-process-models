
# download the noaa once then apply the forecasts
source('download_noaa.R')
message('NOAA downloads complete!')

# Script to run forecasts
source("ignore_sigpipes.R")
source("./Models/processModelFunctions.R")
source('./Models/procCTMIMonod.R')
message('procCTMIMonod model submitted')
