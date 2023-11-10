# set reference datetime
curr_reference_datetime <- Sys.Date() - 1
message(paste("creating forecasts for",print(curr_reference_datetime)))

# download the noaa once then apply the forecasts
source('download_noaa.R')
message('NOAA downloads complete!')

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
