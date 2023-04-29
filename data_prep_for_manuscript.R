
library(tidyverse)
library(tictoc)
library(zoo)

# Source function for phenology fitting, tests of fit, and plot generation
source(here::here("phenology_fit_tester.R"))

# Load data from simulation
load(here::here("phenology_fit_tester_output.RData"))
test_output = bind_rows(test_output)

# First off, lets print some example plots of phenology fits for given datasets
#   FIRST with no cloud cover
test_cloud_fraction <- 0

l8_fit_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l8_plot <- l8_fit_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(l8_fit_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(l8_fit_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(l8_fit_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(l8_fit_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
l8_plot
ggsave(here::here("l8_phenology_retrieval_example.png"), l8_plot, width=6, height=4)

s2_fit_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 5, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
s2_plot <- s2_fit_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(s2_fit_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(s2_fit_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(s2_fit_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(s2_fit_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
s2_plot
ggsave(here::here("s2_phenology_retrieval_example.png"), s2_plot, width=6, height=4)

l5_fit_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 10, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l5_plot <- l5_fit_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(l5_fit_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(l5_fit_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(l5_fit_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(l5_fit_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
l5_plot
ggsave(here::here("l5_phenology_retrieval_example.png"), l5_plot, width=6, height=4)

planet_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 5, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 2, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
planet_plot <- planet_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(planet_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(planet_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(planet_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(planet_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
planet_plot
ggsave(here::here("planet_phenology_retrieval_example.png"), planet_plot, width=6, height=4)


# NOW let's do the same, but with a cloud cover fraction of 50%!

test_cloud_fraction <- 0.5

l8_fit_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l8_plot <- l8_fit_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(l8_fit_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(l8_fit_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(l8_fit_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(l8_fit_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
l8_plot
ggsave(here::here("l8_phenology_retrieval_cf_50_example.png"), l8_plot, width=6, height=4)

s2_fit_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 5, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
s2_plot <- s2_fit_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(s2_fit_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(s2_fit_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(s2_fit_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(s2_fit_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
s2_plot
ggsave(here::here("s2_phenology_retrieval_cf_50_example.png"), s2_plot, width=6, height=4)

l5_fit_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 10, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l5_plot <- l5_fit_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(l5_fit_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(l5_fit_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(l5_fit_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(l5_fit_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
l5_plot
ggsave(here::here("l5_phenology_retrieval_cf_50_example.png"), l5_plot, width=6, height=4)

planet_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 5, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 2, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
planet_plot <- planet_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(planet_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(planet_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(planet_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(planet_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
planet_plot
ggsave(here::here("planet_phenology_retrieval_cf_50_example.png"), planet_plot, width=6, height=4)





# And again, but with a cloud cover fraction of 75%!

test_cloud_fraction <- 0.75

l8_fit_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l8_plot <- l8_fit_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(l8_fit_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(l8_fit_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(l8_fit_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(l8_fit_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
l8_plot
ggsave(here::here("l8_phenology_retrieval_cf_75_example.png"), l8_plot, width=6, height=4)

s2_fit_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 5, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
s2_plot <- s2_fit_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(s2_fit_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(s2_fit_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(s2_fit_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(s2_fit_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
s2_plot
ggsave(here::here("s2_phenology_retrieval_cf_75_example.png"), s2_plot, width=6, height=4)

l5_fit_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 10, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l5_plot <- l5_fit_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(l5_fit_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(l5_fit_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(l5_fit_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(l5_fit_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
l5_plot
ggsave(here::here("l5_phenology_retrieval_cf_75_example.png"), l5_plot, width=6, height=4)

planet_example <- getPhenologyFit((test_output %>% filter(signal_to_noise_ratio == 5, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 2, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
planet_plot <- planet_example[[9]] + 
  ylab("Simulated Response Variable") + 
  xlab("Day of Year") + 
  geom_text(aes(x=325, y=1.0, label=as.numeric(planet_example[[1]]["sample_period"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.90, label=as.numeric(planet_example[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
  geom_text(aes(x=325, y=0.75, label=as.numeric(round(planet_example[[1]]["r_sqd"],4)), hjust="left")) + 
  geom_text(aes(x=325, y=0.65, label=as.numeric(round(planet_example[[1]]["rms_error"],4)), hjust="left")) + 
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0))
planet_plot
ggsave(here::here("planet_phenology_retrieval_cf_75_example.png"), planet_plot, width=6, height=4)