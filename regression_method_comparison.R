

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
test_df_linmed <- bind_rows(test_output_linmed)
write_csv(test_df_linmed, file=here::here("all_methods_simulation_results_linear_median.csv"))

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

test_df <- read_csv(here::here("all_methods_simulation_results_multiwindow.csv")) %>%
  mutate(method = "quadratic")
test_df_linmed <- read_csv(here::here("all_methods_simulation_results_linear_median.csv")) %>%
  mutate(method = "linear")
test_df_med <- read_csv(here::here("all_methods_simulation_results_median_only.csv")) %>%
  mutate(method = "median")
test_all_results <- rbind(test_df, test_df_linmed, test_df_med)

# Examples from particular satellites
modis <- test_all_results %>%
  filter(signal_to_noise_ratio == 100,
         fixed_noise == 0.02,
         sample_period == 1) %>%
  mutate(sensor = "modis")
planet <- test_all_results %>%
  filter(signal_to_noise_ratio == 20,
         fixed_noise == 0.05,
         sample_period == 2) %>%
  mutate(sensor = "planet")
sentinel_2 <- test_all_results %>%
  filter(signal_to_noise_ratio == 100,
         fixed_noise == 0.02,
         sample_period == 5) %>%
  mutate(sensor = "sentinel_2")
landsat_8 <- test_all_results %>%
  filter(signal_to_noise_ratio == 100,
         fixed_noise == 0.02,
         sample_period == 16) %>%
  mutate(sensor = "landsat_8")
landsat_89 <- test_all_results %>%
  filter(signal_to_noise_ratio == 100,
         fixed_noise == 0.02,
         sample_period == 8) %>%
  mutate(sensor = "landsat_89")
landsat_5 <- test_all_results %>%
  filter(signal_to_noise_ratio == 20,
         fixed_noise == 0.05,
         sample_period == 16) %>%
  mutate(sensor = "landsat_5")
all_sensors <- rbind(modis, planet, sentinel_2, landsat_89, landsat_8, landsat_5)



# Inter-sensor Accuracy Comparison
sensor_comparison_plot <- ggplot() + 
  geom_line(data = all_sensors %>% filter(method == "quadratic", 
                                          neighborhood_window_width == 30, 
                                          !(sensor %in% c("landsat_5","landsat_89"))) %>%
              group_by(sensor, cloudy_fraction) %>%
              summarize(r_sqd_median = median(r_sqd, na.rm=TRUE)),
            aes(x = cloudy_fraction, y = r_sqd_median), size=1, col="black") +
  geom_line(data = all_sensors %>% filter(method == "quadratic", 
                                          neighborhood_window_width == 30, 
                                          !(sensor %in% c("landsat_5","landsat_89"))) %>%
              group_by(sensor, cloudy_fraction) %>%
              summarize(r_sqd_median = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_median), size=1, col="red", linetype="dashed") + 
  geom_line(data = all_sensors %>% filter(method == "quadratic", 
                                          neighborhood_window_width == 30, 
                                          !(sensor %in% c("landsat_5","landsat_89"))) %>%
              group_by(sensor, cloudy_fraction) %>%
              summarize(r_sqd_median = quantile(r_sqd, 0.95)),
            aes(x = cloudy_fraction, y = r_sqd_median), size=1, col="blue", linetype="dashed") + 
  geom_hline(yintercept=0.8, color="black", linetype="dashed") + 
  scale_x_continuous(limits=c(0,1), breaks=(1:4)/4, expand=c(0,0)) + 
  scale_y_continuous(limits=c(0.6,1), expand=c(0,0)) + 
  facet_wrap(~factor(sensor, levels = c("landsat_8","landsat_89","sentinel_2","planet","modis")), nrow=1) + 
  xlab("Cloud Cover (Fraction of Scenes)") + 
  ylab("R^2")
sensor_comparison_plot
ggsave(here::here("figures","sensor_comparison_distribution.png"),
       width=5.5, height=2)


# Comparison by fitting method (quadratic, linear, median)
#   note that higher order methods still include lower order fits as fallback when 
#   too few points were available
method_comparison_plot <- ggplot() +
  geom_line(data = all_sensors %>% filter(neighborhood_window_width==60, sensor=="landsat_8") %>%
              group_by(method, cloudy_fraction) %>%
              summarize(r_sqd_median = median(r_sqd, na.rm=TRUE)),
            aes(x = cloudy_fraction, y = r_sqd_median), size=1, col="black") +
  geom_line(data = all_sensors %>% filter(neighborhood_window_width==60, sensor=="landsat_8") %>%
              group_by(method, cloudy_fraction) %>%
              summarize(r_sqd_median = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_median), size=1, col="red", linetype="dashed") +
  geom_line(data = all_sensors %>% filter(neighborhood_window_width==60, sensor=="landsat_8") %>%
              group_by(method, cloudy_fraction) %>%
              summarize(r_sqd_median = quantile(r_sqd, 0.95)),
            aes(x = cloudy_fraction, y = r_sqd_median), size=1, col="blue", linetype="dashed") +
  geom_hline(yintercept=0.8, color="black", linetype="dashed") +
  scale_x_continuous(limits=c(0,1), breaks=(1:4)/4, expand=c(0,0)) +
  scale_y_continuous(limits=c(0.6,1), expand=c(0,0)) +
  facet_wrap(~factor(method, levels = c("median","linear","quadratic")), nrow=1) +
  xlab("Cloud Cover (Fraction of Scenes)") +
  ylab("R^2")
method_comparison_plot
ggsave(here::here("figures","method_comparison_distribution.png"),
       method_comparison_plot,
       width=4.25, height=2)


# Finally, compare across neighbor window widths
window_comparison_plot <- ggplot() +
  geom_line(data = all_sensors %>% filter(method=="quadratic", sensor=="landsat_8") %>%
              group_by(neighborhood_window_width, cloudy_fraction) %>%
              summarize(r_sqd_median = median(r_sqd, na.rm=TRUE)),
            aes(x = cloudy_fraction, y = r_sqd_median), size=1, col="black") +
  geom_line(data = all_sensors %>% filter(method=="quadratic", sensor=="landsat_8") %>%
              group_by(neighborhood_window_width, cloudy_fraction) %>%
              summarize(r_sqd_median = quantile(r_sqd, 0.05)),
            aes(x = cloudy_fraction, y = r_sqd_median), size=1, col="red", linetype="dashed") +
  geom_line(data = all_sensors %>% filter(method=="quadratic", sensor=="landsat_8") %>%
              group_by(neighborhood_window_width, cloudy_fraction) %>%
              summarize(r_sqd_median = quantile(r_sqd, 0.95)),
            aes(x = cloudy_fraction, y = r_sqd_median), size=1, col="blue", linetype="dashed") +
  geom_hline(yintercept=0.8, color="black", linetype="dashed") +
  scale_x_continuous(limits=c(0,1), breaks=(1:4)/4, expand=c(0,0)) +
  scale_y_continuous(limits=c(0.6,1), expand=c(0,0)) +
  facet_wrap(~factor(neighborhood_window_width, levels = as.character((1:6)*10)), nrow=1) +
  xlab("Cloud Cover (Fraction of Scenes)") +
  ylab("R^2")
window_comparison_plot
ggsave(here::here("figures","window_comparison_distribution.png"),
       window_comparison_plot,
       width=4.25, height=2)


