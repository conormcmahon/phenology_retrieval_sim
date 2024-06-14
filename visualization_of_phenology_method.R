
library(tidyverse)
library(here)

source(here::here("phenology_fit_tester.R"))

# Choose an example simulated dataset 
example_datasets <- read_csv(here::here("all_methods_simulation_results.csv")) %>%
  filter(spring_rate == 10,
         fall_rate == 10,
         fall_doy == 270,
         neighborhood_window_width == 20,
         cloudy_fraction == 0.25,
         sample_period == 5,
         fixed_noise == 0.02, 
         signal_to_noise_ratio == 100)

phenology_dataset <- getPhenologyFit(example_datasets, return_data_series = TRUE, num_fitting_points = 12)

# Output from getPhenologyFit function:
# List of: 
#   1) output_dataframe,
#   2) overpass_doys, 
#   3) ndvi_reference, 
#   4) overpass_doys_cloudless, 
#   5) ndvi_cloudless,
#   6) fitting_points, 
#   7) ndvi_fit, 
#   8) ndvi_reference,
#   9) plot_fit
#   10) ndvi_with_noise


example_cloudfree <- data.frame(doy = phenology_dataset[[4]],
                                ndvi = phenology_dataset[[5]])

example_cloudy_overpasses <- data.frame(doy = phenology_dataset[[2]],
                                        ndvi = rep(0, length(phenology_dataset[[2]]))) %>%
  filter(!(doy %in% example_cloudfree$doy))

fitted_line <- data.frame(doy = phenology_dataset[[6]],
                          ndvi = phenology_dataset[[7]]$fitted_value)


# First, a plot showing entire hypothetical timeseries
points_too_low <- c(21,23,30)
plot_1 <- ggplot() + 
  geom_point(data = example_cloudy_overpasses,
             aes(x=doy, y=ndvi), col="red") + 
  geom_point(data = example_cloudfree[-points_too_low,],
             aes(x=doy, y=ndvi)) + 
  geom_point(data = example_cloudfree[points_too_low,],
             aes(x=doy, y=ndvi-0.2), col="purple") + 
  xlab("Day of Year") + 
  ylab("NDVI") + 
  theme_bw() + 
  scale_x_continuous(limits=c(0,370), expand=c(0,0))
plot_1
ggsave(here::here("output_plots","fig_1_subplot_1.png"), 
       plot_1, width=4.5, height=3)

# Next, zoomed in on cloud-contaminated point
points_too_low <- c(21,23,30)
plot_2 <- ggplot() + 
  geom_point(data = example_cloudy_overpasses,
             aes(x=doy, y=ndvi), col="red") + 
  geom_point(data = example_cloudfree[-points_too_low,],
             aes(x=doy, y=ndvi)) + 
  geom_point(data = example_cloudfree[points_too_low,],
             aes(x=doy, y=ndvi-0.2), col="purple") + 
  xlab("Day of Year") + 
  ylab("NDVI") + 
  theme_bw() + 
  scale_x_continuous(limits=c(180,220)) + 
  scale_y_continuous(limits=c(0.5,1.0), expand=c(0,0))
plot_2
ggsave(here::here("output_plots","fig_1_subplot_2.png"), 
       plot_2, width=4.5, height=3)

# Next, all the filtered data with 12 monthly timesteps 
plot_3 <- ggplot() + 
  geom_point(data = example_cloudfree[-points_too_low,],
             aes(x=doy, y=ndvi)) + 
  geom_vline(xintercept=phenology_dataset[[6]], col="black", linetype="dashed") + 
  xlab("Day of Year") + 
  ylab("NDVI") + 
  theme_bw() + 
  scale_x_continuous(limits=c(0,370), expand=c(0,0),
                     breaks=round(phenology_dataset[[6]]), 
                     minor_breaks = NULL)
plot_3
ggsave(here::here("output_plots","fig_1_subplot_3.png"), 
       plot_3, width=4.5, height=3)

# Curve fitting - testing median, linear regression, and quadratic regression 

# Next, zoomed-in comparison of fitting methods 
# Set target point and window for analysis
target_doy <- 61
window_width <- 30
# Get points in window
neighborhood_points <- example_cloudfree[-points_too_low,] %>%
  filter(doy >= target_doy - window_width,
         doy < target_doy + window_width) %>%
  mutate(doy_sq = doy^2)
# Fit quadratic, linear, and median predictions
quad_reg <- lm(data = neighborhood_points,
               formula = ndvi ~ doy + doy_sq)
lin_reg <- lm(data = neighborhood_points,
              formula = ndvi ~ doy)
median_pred <- median(neighborhood_points$ndvi)
linear_pred <- predict(lin_reg, newdata=data.frame(doy=target_doy))
quadratic_pred <- predict(quad_reg, newdata=data.frame(doy=target_doy, doy_sq=target_doy^2))

predicted_dataframe <- data.frame(doy = 1:365) %>%
  mutate(doy_sq = doy^2)
predicted_dataframe$linear_ndvi <- predict(lin_reg, 
                                           newdata=predicted_dataframe)
predicted_dataframe$quadratic_ndvi <- predict(quad_reg, 
                                              newdata=predicted_dataframe)

# Create plot
plot_4 <- ggplot() + 
  geom_point(data = example_cloudfree[-points_too_low,],
             aes(x=doy, y=ndvi)) + 
  geom_point(data = neighborhood_points,
             aes(x=doy, y=ndvi)) + 
  geom_point(aes(x = target_doy, y=median_pred), col="cyan3",size=3) +
  geom_point(aes(x = target_doy, y=linear_pred), col="darkgreen", size=3) +
  geom_point(aes(x = target_doy, y=quadratic_pred), col="blue", size=3) +
  geom_line(data=predicted_dataframe,
            aes(x=doy, y=linear_ndvi), col="darkgreen", size=0.5) + 
  geom_line(data=predicted_dataframe,
            aes(x=doy, y=quadratic_ndvi), col="blue", size=0.5) + 
  geom_vline(xintercept = target_doy, col="black", linetype="dashed") + 
  xlab("Day of Year") + 
  ylab("NDVI") + 
  theme_bw() + 
  scale_x_continuous(limits=c(target_doy-window_width,target_doy+window_width)) + 
  scale_y_continuous(limits=c(0.1,0.6), expand=c(0,0))
plot_4
ggsave(here::here("output_plots","fig_1_subplot_4.png"), 
       plot_4, width=4.5, height=3)

# Finally, the fitted phenology curve
plot_5 <- ggplot() + 
  geom_point(data = example_cloudfree[-points_too_low,],
             aes(x=doy, y=ndvi)) + 
  geom_line(data = fitted_line,
            aes(x=doy, y=ndvi), col="blue") +
  geom_point(data = fitted_line,
             aes(x=doy, y=ndvi), col="blue", size=3) +
  xlab("Day of Year") + 
  ylab("NDVI") + 
  theme_bw() + 
  scale_x_continuous(limits=c(0,370), expand=c(0,0),
                     breaks=round(phenology_dataset[[6]]), 
                     minor_breaks = NULL)
plot_5
ggsave(here::here("output_plots","fig_1_subplot_5.png"), 
       plot_5, width=4.5, height=3)

