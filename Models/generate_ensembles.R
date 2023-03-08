# Generate some ensemble forecasts based on those submitted

# 1. Start by looking in the bucket and grabbing the forecasts
# 2. Subsample the forecast to an equal ensemble size
# 3. Check that it's in the EFI format
# 4. Submit forecast to challenge!
setwd(here::here())
library(tidyverse)
library(arrow)
library(lubridate)
source('./R/create_mme.R')

Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")
Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")

# where are the raw forecasts
s3 <- s3_bucket("neon4cast-forecasts/raw/aquatics", 
                endpoint_override= "data.ecoforecast.org")

# when do you want to generate the MMEs for
forecast_date <- as.character(Sys.Date() - 1)

# generate ensembles
if (dir.exists('./Forecasts/ensembles') != T) {
  dir.create('./Forecasts/ensembles', recursive = T)
}

# Ensemble 1 = empirical baselines (RW + climatology)
create_mme(forecast_models = c('persistenceRW',
                               'climatology'),
           ensemble_name = 'baseline_ensemble',
           forecast_date = forecast_date, 
           s3 = s3, n = 200)


# Submit new ensemble forecasts
todays_ensembles <- list.files('./Forecasts/ensembles', pattern = forecast_date)

for (i in 1:length(todays_ensembles)) {
  neon4cast::submit(file.path('./Forecasts/ensembles', todays_ensembles[i]), ask = F)
}


