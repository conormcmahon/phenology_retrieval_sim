
library(tidyverse)
library(tictoc)
library(zoo)

parameter_values <- crossing(spring_rate = c(2,5,10,20,30), 
                             spring_doy = 90, 
                             fall_rate = c(2,5,10,20,30), 
                             fall_doy = c(150,180,210,240,270,300), 
                             signal_to_noise_ratio = c(1, 2, 5, 10, 20, 100), 
                             fixed_noise = c(0.01, 0.02, 0.05, 0.1),
                             sample_period = c(1,2,5,8,16,365/12), 
                             neighborhood_window_width = c(10, 20, 30, 40, 50, 60), 
                             cloudy_fraction = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95))
parameter_values$seed_index <- 1:nrow(parameter_values)

write_csv(parameter_values, here::here("phenology_fit_test_parameters.csv"))

# Updates 
#   separate fit generation into standalone function
#   separate code to generate plot into standalone function
#   make fitting code return ONLY the fit parameters (not the values or plot itself)
#   write code to display plots for a set of fit parameters

getPhenologyFit <- function(parameter_block, return_data_series=FALSE, try_quadratic=TRUE, try_linear=TRUE, soil_spec=c(0.24,0.4), leaf_spec=c(0.05,0.5))
{
  index_seed <- as.numeric(parameter_block['seed_index'])
  if(index_seed %% 100 == 0)
    print(paste("Reached ", index_seed, "th sample test out of ", nrow(parameter_values), ", or ", index_seed/nrow(parameter_values)*100, "%", sep=""))
  set.seed(index_seed)
    
  # Varying Parameters
  spring_rate <- as.numeric(parameter_block['spring_rate'])
  spring_doy <- as.numeric(parameter_block['spring_doy'])
  fall_rate <- as.numeric(parameter_block['fall_rate'])
  fall_doy <- as.numeric(parameter_block['fall_doy'])
  fixed_noise <- as.numeric(parameter_block['fixed_noise'])
  signal_to_noise_ratio <- as.numeric(parameter_block['signal_to_noise_ratio'])
  sample_period <- as.numeric(parameter_block['sample_period'])
  neighborhood_window_width <- as.numeric(parameter_block['neighborhood_window_width'])
  cloudy_fraction <- as.numeric(parameter_block['cloudy_fraction'])
  
  # Fixed Parameters
  num_fitting_points <- 52
  max_greenness <- 0.9
  #noise_sd <- max_greenness / signal_to_noise_ratio
  
  overpass_doys <- (1:floor(365/sample_period))*sample_period
  
  # Double-logistic Function for Phenology Examples
  #   Fitting phenology as linear spectral combination of leaf and soil spectra
  #   Varies from 100% soil to 100% leaf, seasonally 
  leaf_fraction <- (plogis(overpass_doys,location=spring_doy,scale=spring_rate) - plogis(overpass_doys,location=fall_doy,scale=fall_rate))
  red_refl <- (1-leaf_fraction) * soil_spec[1] + leaf_fraction * leaf_spec[1]
  NIR_refl <- (1-leaf_fraction) * soil_spec[2] + leaf_fraction * leaf_spec[2]
  # Add noise terms to red and NIR reflectances
  red_refl_with_noise <- red_refl + rnorm(length(red_refl), 
                                          mean=0, 
                                          sd=abs(red_refl/signal_to_noise_ratio/2 + fixed_noise))
  NIR_refl_with_noise <- NIR_refl + rnorm(length(NIR_refl), 
                                          mean=0, 
                                          sd=abs(NIR_refl/signal_to_noise_ratio/2 + fixed_noise))
  # Correct for impossible reflectances (must vary from 0 to 1)
  red_refl_with_noise[red_refl_with_noise < 0] <- 0.01
  NIR_refl_with_noise[NIR_refl_with_noise < 0] <- 0.01
  red_refl_with_noise[red_refl_with_noise > 1] <- 1
  NIR_refl_with_noise[NIR_refl_with_noise > 1] <- 1
  # Add Gaussian noise to phenology 
  ndvi_with_noise <- (NIR_refl_with_noise - red_refl_with_noise) / (NIR_refl_with_noise + red_refl_with_noise)
  
  # Remove samples that are cloudy
  clouds <- sample(1:length(ndvi_with_noise),
                   length(ndvi_with_noise)*cloudy_fraction)
  if(length(clouds) != 0)
  {
    ndvi_cloudless <- ndvi_with_noise[-clouds]
    overpass_doys_cloudless <- overpass_doys[-clouds]
  }else
  {
    ndvi_cloudless <- ndvi_with_noise
    overpass_doys_cloudless <- overpass_doys
  }
  
  # Get points at which to fit the phenology curve
  fitting_points <- (1:num_fitting_points)*(365/num_fitting_points)
  # True NDVI curve
  leaf_fraction_reference <- (plogis(fitting_points,location=spring_doy,scale=spring_rate) - plogis(fitting_points,location=fall_doy,scale=fall_rate))
  red_refl_reference <- (1-leaf_fraction_reference) * soil_spec[1] + leaf_fraction_reference * leaf_spec[1]
  NIR_refl_reference <- (1-leaf_fraction_reference) * soil_spec[2] + leaf_fraction_reference * leaf_spec[2]
  ndvi_reference <- (NIR_refl_reference - red_refl_reference) / (NIR_refl_reference + red_refl_reference)
  
  
  
  fitAtPoint <- function(target_doy, phenology, doy_vector, window_width)
  {
    neighbor_indices <- which(abs((doy_vector - target_doy)) < window_width)
    neighbor_indices <- c(neighbor_indices, 
                          which(abs((doy_vector-365 - target_doy)) < window_width))
    neighborhood <- data.frame(neighborhood_values = phenology[neighbor_indices], 
                               neighborhood_doys = doy_vector[neighbor_indices],
                               neighborhood_doys_squared = (doy_vector[neighbor_indices]) ^ 2) %>%
      drop_na()
    if(nrow(neighborhood)==0)
      return(NA)
    median_value <- median(neighborhood$neighborhood_values, na.rm=TRUE)
    neighborhood_mean <- mean(neighborhood$neighborhood_values, na.rm=TRUE)
    neighborhood_stdev <- sd(neighborhood$neighborhood_values, na.rm=TRUE)
    
    # Check to make sure there are enough data points to fit a quadratic
    if((nrow(neighborhood) >= 6) & (try_quadratic))
    {
      quadratic_fit <- lm(data=neighborhood, neighborhood_values ~ neighborhood_doys_squared+neighborhood_doys)
      quadratic_prediction <- predict.lm(quadratic_fit, data.frame(neighborhood_doys=target_doy, neighborhood_doys_squared=target_doy^2))
      # check to make sure value is physically reasonable
      if((quadratic_prediction > -1) & (quadratic_prediction < 1))
        # Check to make sure value is statistically reasonable
        if(abs(quadratic_prediction-neighborhood_mean) < 1.5*neighborhood_stdev)
          return(as.numeric(quadratic_prediction))
    }
    # If the above conditions aren't met, try a linear prediction:
    if((nrow(neighborhood) >= 3) & (try_linear))
    {
      linear_fit <- lm(data=neighborhood, neighborhood_values ~ neighborhood_doys)
      linear_prediction <- predict.lm(linear_fit, data.frame(neighborhood_doys=target_doy, neighborhood_doys_squared=target_doy^2))
      # check to make sure value is physically reasonable
      if((linear_prediction > -1) & (linear_prediction < 1))
        # Check to make sure value is statistically reasonable
        if(abs(linear_prediction-neighborhood_mean) < 1.5*neighborhood_stdev)
          return(as.numeric(linear_prediction))
    }
    return(as.numeric(median_value))
  }
  
  tic('fitting_time')
  ndvi_fit <- unlist(lapply(fitting_points, fitAtPoint, phenology=ndvi_cloudless, doy_vector=overpass_doys_cloudless, window_width=neighborhood_window_width))
  
  # Final linear gap-filling in phenology fit
  ndvi_fit <- na.approx(c(ndvi_fit,ndvi_fit,ndvi_fit))[(2+length(fitting_points)-min(which(!is.na(ndvi_fit)))):(1+2*length(fitting_points)-min(which(!is.na(ndvi_fit))))]
  fitting_time <- toc(quiet=TRUE)
  
  # Some Error Metrics...
  # RMS
  rms_error <- sqrt(mean((ndvi_fit - ndvi_reference)^2))
  #print(paste("Root mean square error between fit and reference: ", rms_error, sep=""))
  # Linear Correlation (predicted vs. actual)
  model_correlation <- lm(ndvi_reference ~ ndvi_fit)
  
  output_dataframe <- data.frame(seed_index = index_seed,
                                 spring_rate = spring_rate,
                                 spring_doy = spring_doy,
                                 fall_rate = fall_rate,
                                 fall_doy = fall_doy,
                                 signal_to_noise_ratio = signal_to_noise_ratio,
                                 fixed_noise = fixed_noise,
                                 sample_period = sample_period,
                                 neighborhood_window_width = neighborhood_window_width,
                                 cloudy_fraction = cloudy_fraction,
                                 rms_error = rms_error,
                                 r_sqd = summary(model_correlation)$adj.r.squared,
                                 scale = model_correlation$coefficients[2],
                                 intercept = model_intercept <- model_correlation$coefficients[1],
                                 execution_time = fitting_time$toc - fitting_time$tic)
  
  if(!return_data_series)  
    return(output_dataframe)

  fitting_data <- data.frame(dates=fitting_points, fitted=ndvi_fit, reference=ndvi_reference)
  retrieved_data <- data.frame(doy=overpass_doys_cloudless, greenness=ndvi_cloudless)
  
  # Generate output plot comparing phenology prediction to reference
  plot_fit <- ggplot() + 
    geom_point(data=retrieved_data,
               aes(x=doy, y=greenness), col="blue", size=1) + 
    geom_line(data=fitting_data, 
              aes(x=dates,y=fitted), col="red") + 
    geom_line(data=fitting_data, 
              aes(x=dates,y=reference))
    
  return(list(output_dataframe,
              overpass_doys, ndvi_reference, 
              overpass_doys_cloudless, ndvi_cloudless,
              fitting_points, ndvi_fit, ndvi_reference,
              plot_fit))
}


