# function to generate a balance MME from a list of forecast model names

create_mme <- function(forecast_models, # vector of list of model names
                       n = 200, # what size ensemble do you want?
                       ensemble_name, # what is the name of the ensemble output
                       forecast_date, # when is the forecast for (what forecast to grab)
                       s3 = s3) # the s3 region to look for the forecasts
  {
  n_models <- length(forecast_models)
  sample <- round(n / n_models, digits = 0)
  
  mme_forecast <- NULL
  for (i in 1:length(forecast_models)) {
    
    forecast_file <- paste0('aquatics-', forecast_date, '-', forecast_models[i], '.csv.gz')
    forecast <- read4cast::read_forecast(forecast_file, s3 = s3)  |>  
      filter(variable == 'temperature')
    message(forecast_models[i], ' read in')
    
    
    forecast_sample <- forecast %>%
      distinct(parameter) %>%
      slice_sample(n = sample) %>%
      left_join(., forecast, by = c("parameter")) %>%
      mutate(model_id = ensemble_name) %>%
      group_by(site_id, model_id, reference_datetime)
    
    mme_forecast <- bind_rows(mme_forecast, forecast_sample) 
    
  }
  filename <- paste0('aquatics-', forecast_date, '-', ensemble_name, '.csv.gz')
  
  mme_forecast |>
    readr::write_csv(file.path('./Forecasts/ensembles', filename))
  
  message(ensemble_name, ' generated')
}
