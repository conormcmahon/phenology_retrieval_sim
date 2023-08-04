

# Comparisons between: 
#    quadratic + linear + median fits 
#    linear + median fits
#    median fits

library(tidyverse)

# ******** NOTE ********
# These processes take a very long time to run and generate the data, 
#   so comment out the next several code blocks after the first run and just read in from saved files!
# /* start comment block here

# Generate results using ALL methods
test_output <- apply(parameter_values, getPhenologyFit, MARGIN=1, 
                         return_data_series=FALSE, 
                         try_quadratic=TRUE, 
                         try_linear=TRUE,
                         soil_spec=c(0.24,0.4), 
                         leaf_spec=c(0.05,0.5))
test_df <- bind_rows(test_output)
write_csv(test_df, file=here::here("all_methods_simulation_results_multiwindow.csv"))

# Generate results using linear and median methods
test_output_linmed <- apply(parameter_values, getPhenologyFit, MARGIN=1, 
                         return_data_series=FALSE, 
                         try_quadratic=FALSE, 
                         try_linear=TRUE,
                         soil_spec=c(0.24,0.4), 
                         leaf_spec=c(0.05,0.5))
test_df_kinmed <- bind_rows(test_output_linmed)
write_csv(test_df_kinmed, file=here::here("all_methods_simulation_results_linear_median.csv"))

# Generate results using only median method
test_output_med <- apply(parameter_values, getPhenologyFit, MARGIN=1, 
                         return_data_series=FALSE, 
                         try_quadratic=FALSE, 
                         try_linear=FALSE,
                         soil_spec=c(0.24,0.4), 
                         leaf_spec=c(0.05,0.5))
test_df_med <- bind_rows(test_output_med)
write_csv(test_df_med, file=here::here("all_methods_simulation_results_median_only.csv"))

# */ # after initial run, uncomment this line to remove this block of code, which is slow to run

test_df <- read_csv(here::here("all_methods_simulation_results_multiwindow.csv"))
test_df_linmed <- read_csv(here::here("all_methods_simulation_results_linear_median.csv"))
test_df_med <- read_csv(here::here("all_methods_simulation_results_median_only.csv"))

# First, vizualize results for Landsat
ggplot() + 
  geom_line(data = test_df %>% filter(signal_to_noise_ratio == 100,
                                      fixed_noise == 0.02,
                                      sample_period == 16,
                                      neighborhood_window_width == 30) %>%
              group_by(cloudy_fraction) %>%
              summarize(r_sqd_05 = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_05)) + 
  geom_line(data = test_df_linmed %>% filter(signal_to_noise_ratio == 100,
                                             fixed_noise == 0.02,
                                             sample_period == 16,
                                             neighborhood_window_width == 30) %>%
              group_by(cloudy_fraction) %>%
              summarize(r_sqd_05 = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_05),
            color="red") + 
  geom_line(data = test_df_med %>% filter(signal_to_noise_ratio == 100,
                                          fixed_noise == 0.02,
                                          sample_period == 16,
                                          neighborhood_window_width == 30) %>%
              group_by(cloudy_fraction) %>%
              summarize(r_sqd_05 = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_05),
            color="blue") + 
  scale_y_continuous(limits=c(0,1))


# Next, vizualize results for Landsat 8 + 9
ggplot() + 
  geom_line(data = test_df %>% filter(signal_to_noise_ratio == 100,
                                      fixed_noise == 0.02,
                                      sample_period == 8,
                                      neighborhood_window_width == 30) %>%
              group_by(cloudy_fraction) %>%
              summarize(r_sqd_05 = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_05)) + 
  geom_line(data = test_df_linmed %>% filter(signal_to_noise_ratio == 100,
                                             fixed_noise == 0.02,
                                             sample_period == 8,
                                             neighborhood_window_width == 30) %>%
              group_by(cloudy_fraction) %>%
              summarize(r_sqd_05 = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_05),
            color="red") + 
  geom_line(data = test_df_med %>% filter(signal_to_noise_ratio == 100,
                                          fixed_noise == 0.02,
                                          sample_period == 8,
                                          neighborhood_window_width == 30) %>%
              group_by(cloudy_fraction) %>%
              summarize(r_sqd_05 = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_05),
            color="blue") + 
  scale_y_continuous(limits=c(0,1))


# Next, vizualize results for Sentinel-2
ggplot() + 
  geom_line(data = test_df %>% filter(signal_to_noise_ratio == 100,
                                      fixed_noise == 0.02,
                                      sample_period == 5,
                                      neighborhood_window_width == 30) %>%
              group_by(cloudy_fraction) %>%
              summarize(r_sqd_05 = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_05)) + 
  geom_line(data = test_df_linmed %>% filter(signal_to_noise_ratio == 100,
                                             fixed_noise == 0.02,
                                             sample_period == 5,
                                             neighborhood_window_width == 30) %>%
              group_by(cloudy_fraction) %>%
              summarize(r_sqd_05 = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_05),
            color="red") + 
  geom_line(data = test_df_med %>% filter(signal_to_noise_ratio == 100,
                                          fixed_noise == 0.02,
                                          sample_period == 5,
                                          neighborhood_window_width == 30) %>%
              group_by(cloudy_fraction) %>%
              summarize(r_sqd_05 = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_05),
            color="blue") + 
  scale_y_continuous(limits=c(0,1))
