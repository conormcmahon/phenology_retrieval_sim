
library(tidyverse)

source(here::here("phenology_fit_tester.R"))

# Directly estimate phenometrics of double-sigmoid functions from satellite data

landsat_parameters <- crossing(spring_rate = (1:10)*3, 
                               spring_doy = 90, 
                               fall_rate = (1:10)*3, 
                               fall_doy = 120+(1:10)*18, 
                               signal_to_noise_ratio = 100, 
                               fixed_noise = 0.02,
                               sample_period = 8, 
                               neighborhood_window_width = 30, 
                               cloudy_fraction = (0:19)/20)
landsat_parameters$seed_index <- 1:nrow(landsat_parameters)

landsat_example <- apply(landsat_parameters, getPhenologyFit, MARGIN=1, 
                         return_data_series=TRUE, 
                         try_quadratic=TRUE, 
                         try_linear=TRUE,
                         soil_spec=c(0.24,0.4), 
                         leaf_spec=c(0.05,0.5),
                         total_num_tests=nrow(landsat_parameters))

fit_dataframe <- lapply(landsat_example, 
                        function(data_list){
                          new_data <- data_list[[7]]
                          new_data$reference <- data_list[[8]]
                          new_data <- cbind(new_data, data_list[[1]])
                        })
fit_dataframe <- bind_rows(fit_dataframe) %>% 
  mutate(error = fitted_value - reference,
         num_window_samples = samples_before + samples_after)

ggplot(fit_dataframe) + 
geom_density2d_filled(aes(x=(samples_before+samples_after), y=abs(fitted_value-reference)))


