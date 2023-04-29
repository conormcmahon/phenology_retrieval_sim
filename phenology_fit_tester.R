
library(tidyverse)
library(tictoc)
library(zoo)

parameter_values <- crossing(spring_rate = c(2,5,10,20,30), 
                             spring_doy = 90, 
                             fall_rate = c(2,5,10,20,30), 
                             fall_doy = c(150,180,210,240,270,300), 
                             signal_to_noise_ratio = c(0.5, 1, 2, 5, 10, 20), 
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

getPhenologyFit <- function(parameter_block, return_data_series=FALSE, try_quadratic=TRUE, try_linear=TRUE)
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
  signal_to_noise_ratio <- as.numeric(parameter_block['signal_to_noise_ratio'])
  sample_period <- as.numeric(parameter_block['sample_period'])
  neighborhood_window_width <- as.numeric(parameter_block['neighborhood_window_width'])
  cloudy_fraction <- as.numeric(parameter_block['cloudy_fraction'])
  
  # Fixed Parameters
  num_fitting_points <- 52
  max_greenness <- 0.9
  noise_sd <- max_greenness / signal_to_noise_ratio
  
  overpass_doys <- (1:floor(365/sample_period))*sample_period
  
  # Double-logistic Function for Phenology Examples
  random_phenology <- max_greenness * (plogis(overpass_doys,location=spring_doy,scale=spring_rate) - plogis(overpass_doys,location=fall_doy,scale=fall_rate))
  # Add Gaussian noise to phenology 
  random_phenology_with_noise <- random_phenology + rnorm(length(random_phenology), mean=0, sd=noise_sd)
  
  # Remove samples that are cloudy
  clouds <- sample(1:length(random_phenology_with_noise),
                   length(random_phenology_with_noise)*cloudy_fraction)
  if(length(clouds) != 0)
  {
    random_phenology_cloudless <- random_phenology_with_noise[-clouds]
    overpass_doys_cloudless <- overpass_doys[-clouds]
  }
  else
  {
    random_phenology_cloudless <- random_phenology_with_noise
    overpass_doys_cloudless <- overpass_doys
  }
  
  fitting_points <- (1:num_fitting_points)*(365/num_fitting_points)
  
  # Get curve representing actual phenology at sampling point intervals
  random_phenology_reference <- max_greenness * (plogis(fitting_points,location=spring_doy,scale=spring_rate) - plogis(fitting_points,location=fall_doy,scale=fall_rate))
  
  # Plot Example Phenology Curve and Noisy Version
  #lines(fitting_points, random_phenology_reference)
  #points(overpass_doys, random_phenology_with_noise, col="red")
  
  fitAtPoint <- function(target_doy, phenology, doy_vector, window_width)
  {
    neighbor_indices <- which(abs((doy_vector - target_doy)) < window_width)
    neighbor_indices <- c(neighbor_indices, 
                          which(abs((doy_vector-365 - target_doy)) < window_width))
    neighborhood <- data.frame(neighborhood_values = phenology[neighbor_indices], 
                               neighborhood_doys = doy_vector[neighbor_indices],
                               neighborhood_doys_squared = (doy_vector[neighbor_indices]) ^ 2)
    median_value <- median(neighborhood$neighborhood_values)
    neighborhood_mean <- mean(neighborhood$neighborhood_values)
    neighborhood_stdev <- sd(neighborhood$neighborhood_values)
    
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
  phenology_fit <- unlist(lapply(fitting_points, fitAtPoint, phenology=random_phenology_cloudless, doy_vector=overpass_doys_cloudless, window_width=neighborhood_window_width))
  
  # Final linear gap-filling in phenology fit
  phenology_fit <- na.approx(c(phenology_fit,phenology_fit,phenology_fit))[(1+length(fitting_points)):(2*length(fitting_points))]
  fitting_time <- toc(quiet=TRUE)
  
  # Some Error Metrics...
  # RMS
  rms_error <- sqrt(mean((phenology_fit - random_phenology_reference)^2))
  #print(paste("Root mean square error between fit and reference: ", rms_error, sep=""))
  # Linear Correlation (predicted vs. actual)
  model_correlation <- lm(random_phenology_reference ~ phenology_fit)
  
  output_dataframe <- data.frame(seed_index = index_seed,
                                 spring_rate = spring_rate,
                                 spring_doy = spring_doy,
                                 fall_rate = fall_rate,
                                 fall_doy = fall_doy,
                                 signal_to_noise_ratio = signal_to_noise_ratio,
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

  # Generate output plot comparing phenology prediction to reference
  plot_fit <- ggplot(data=data.frame(dates=fitting_points, fitted=phenology_fit, reference=random_phenology_reference)) + 
    geom_line(aes(x=dates,y=fitted), col="red") + 
    geom_line(aes(x=dates,y=reference)) + 
    geom_point(data=data.frame(doy=overpass_doys_cloudless, greenness=random_phenology_cloudless),
               aes(x=doy, y=random_phenology_cloudless), col="blue")
    
  return(list(output_dataframe,
              overpass_doys, random_phenology_reference, 
              overpass_doys_cloudless, random_phenology_cloudless,
              fitting_points, phenology_fit, random_phenology_reference,
              plot_fit))
}
