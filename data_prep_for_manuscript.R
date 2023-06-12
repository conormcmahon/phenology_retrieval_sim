
library(tidyverse)
library(tictoc)
library(zoo)

# Source function for phenology fitting, tests of fit, and plot generation
source(here::here("phenology_fit_tester.R"))

# Load data from simulation
#load(here::here("simulation_results_all_methods.rdata"))

buildPlot <- function(input)
{
  plot <- input[[9]] + 
    ylab("Simulated Response Variable") + 
    xlab("Day of Year") + 
    geom_text(aes(x=325, y=1.0, label=as.numeric(input[[1]]["sample_period"]), hjust="left")) + 
    geom_text(aes(x=325, y=0.90, label=as.numeric(input[[1]]["signal_to_noise_ratio"]), hjust="left")) + 
    geom_text(aes(x=325, y=0.75, label=as.numeric(round(input[[1]]["r_sqd"],4)), hjust="left")) + 
    geom_text(aes(x=325, y=0.65, label=as.numeric(round(input[[1]]["rms_error"],4)), hjust="left")) + 
    scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
    scale_x_continuous(expand=c(0,0))
  return(plot)
}
buildPlotSmall <- function(input)
{
  plot <- input[[9]] + 
    geom_text(aes(x=250, y=1.25, label=as.numeric(round(input[[1]]["r_sqd"],4)), hjust="left")) + 
    geom_text(aes(x=250, y=-0.3, label=as.numeric(round(input[[1]]["rms_error"],4)), hjust="left")) + 
    scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
    scale_x_continuous(expand=c(0,0)) + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  return(plot)
}
  
# ***********************************************************************************************
# First off, lets print some example plots of phenology fits for given datasets
#   FIRST with no cloud cover
# ***********************************************************************************************

set.seed(1)

test_cloud_fraction <- 0

l89_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 8, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l89_plot <- buildPlot(l89_fit_example)
l89_plot
l89_plot_small <- buildPlotSmall(l89_fit_example)
ggsave(here::here("figures","l89_phenology_retrieval_00_example.png"), l89_plot, width=6, height=4)
ggsave(here::here("figures","l89_phenology_retrieval_00_example_small.png"), l89_plot_small, width=2.5, height=2)


l8_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l8_plot <- buildPlot(l8_fit_example)
l8_plot
l8_plot_small <- buildPlotSmall(l8_fit_example)
ggsave(here::here("figures","l8_phenology_retrieval_00_example.png"), l8_plot, width=6, height=4)
ggsave(here::here("figures","l8_phenology_retrieval_00_example_small.png"), l8_plot_small, width=2.5, height=2)

s2_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 5, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
s2_plot
s2_plot <- buildPlot(s2_fit_example)
s2_plot
s2_plot_small <- buildPlotSmall(s2_fit_example)
ggsave(here::here("figures","s2_phenology_retrieval_00_example.png"), s2_plot, width=6, height=4)
ggsave(here::here("figures","s2_phenology_retrieval_00_example_small.png"), s2_plot_small, width=2.5, height=2)


l5_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 10, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l5_plot
l5_plot <- buildPlot(l5_fit_example)
l5_plot
l5_plot_small <- buildPlotSmall(l5_fit_example)
ggsave(here::here("figures","l5_phenology_retrieval_00_example.png"), l5_plot, width=6, height=4)
ggsave(here::here("figures","l5_phenology_retrieval_00_example_small.png"), l5_plot_small, width=2.5, height=2)

planet_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 5, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 2, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
planet_plot
planet_plot <- buildPlot(planet_fit_example)
planet_plot
planet_plot_small <- buildPlotSmall(planet_fit_example)
ggsave(here::here("figures","planet_phenology_retrieval_00_example.png"), planet_plot, width=6, height=4)
ggsave(here::here("figures","planet_phenology_retrieval_00_example_small.png"), planet_plot_small, width=2.5, height=2)





# ***********************************************************************************************
# NOW let's do the same, but with a cloud cover fraction of 50%!
# ***********************************************************************************************
test_cloud_fraction <- 0.5

l89_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                           sample_period == 8, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l89_plot <- buildPlot(l89_fit_example)
l89_plot
l89_plot_small <- buildPlotSmall(l89_fit_example)
ggsave(here::here("figures","l89_phenology_retrieval_50_example.png"), l89_plot, width=6, height=4)
ggsave(here::here("figures","l89_phenology_retrieval_50_example_small.png"), l89_plot_small, width=2.5, height=2)


l8_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l8_plot <- buildPlot(l8_fit_example)
l8_plot
l8_plot_small <- buildPlotSmall(l8_fit_example)
ggsave(here::here("figures","l8_phenology_retrieval_50_example.png"), l8_plot, width=6, height=4)
ggsave(here::here("figures","l8_phenology_retrieval_50_example_small.png"), l8_plot_small, width=2.5, height=2)

s2_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 5, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
s2_plot
s2_plot <- buildPlot(s2_fit_example)
s2_plot
s2_plot_small <- buildPlotSmall(s2_fit_example)
ggsave(here::here("figures","s2_phenology_retrieval_50_example.png"), s2_plot, width=6, height=4)
ggsave(here::here("figures","s2_phenology_retrieval_50_example_small.png"), s2_plot_small, width=2.5, height=2)


l5_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 10, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l5_plot
l5_plot <- buildPlot(l5_fit_example)
l5_plot
l5_plot_small <- buildPlotSmall(l5_fit_example)
ggsave(here::here("figures","l5_phenology_retrieval_50_example.png"), l5_plot, width=6, height=4)
ggsave(here::here("figures","l5_phenology_retrieval_50_example_small.png"), l5_plot_small, width=2.5, height=2)

planet_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 5, cloudy_fraction == test_cloud_fraction, 
                                                              sample_period == 2, neighborhood_window_width == 30))[100,], 
                                      return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
planet_plot
planet_plot <- buildPlot(planet_fit_example)
planet_plot
planet_plot_small <- buildPlotSmall(planet_fit_example)
ggsave(here::here("figures","planet_phenology_retrieval_50_example.png"), planet_plot, width=6, height=4)
ggsave(here::here("figures","planet_phenology_retrieval_50_example_small.png"), planet_plot_small, width=2.5, height=2)






# ***********************************************************************************************
# And again, but with a cloud cover fraction of 75%!
# ***********************************************************************************************
test_cloud_fraction <- 0.75

l89_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                           sample_period == 8, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l89_plot <- buildPlot(l89_fit_example)
l89_plot
l89_plot_small <- buildPlotSmall(l89_fit_example)
ggsave(here::here("figures","l89_phenology_retrieval_75_example.png"), l89_plot, width=6, height=4)
ggsave(here::here("figures","l89_phenology_retrieval_75_example_small.png"), l89_plot_small, width=2.5, height=2)


l8_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l8_plot <- buildPlot(l8_fit_example)
l8_plot
l8_plot_small <- buildPlotSmall(l8_fit_example)
ggsave(here::here("figures","l8_phenology_retrieval_75_example.png"), l8_plot, width=6, height=4)
ggsave(here::here("figures","l8_phenology_retrieval_75_example_small.png"), l8_plot_small, width=2.5, height=2)

s2_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 5, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
s2_plot
s2_plot <- buildPlot(s2_fit_example)
s2_plot
s2_plot_small <- buildPlotSmall(s2_fit_example)
ggsave(here::here("figures","s2_phenology_retrieval_75_example.png"), s2_plot, width=6, height=4)
ggsave(here::here("figures","s2_phenology_retrieval_75_example_small.png"), s2_plot_small, width=2.5, height=2)


l5_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 10, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l5_plot
l5_plot <- buildPlot(l5_fit_example)
l5_plot
l5_plot_small <- buildPlotSmall(l5_fit_example)
ggsave(here::here("figures","l5_phenology_retrieval_75_example.png"), l5_plot, width=6, height=4)
ggsave(here::here("figures","l5_phenology_retrieval_75_example_small.png"), l5_plot_small, width=2.5, height=2)

planet_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 5, cloudy_fraction == test_cloud_fraction, 
                                                              sample_period == 2, neighborhood_window_width == 30))[100,], 
                                      return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
planet_plot
planet_plot <- buildPlot(planet_fit_example)
planet_plot
planet_plot_small <- buildPlotSmall(planet_fit_example)
ggsave(here::here("figures","planet_phenology_retrieval_75_example.png"), planet_plot, width=6, height=4)
ggsave(here::here("figures","planet_phenology_retrieval_75_example_small.png"), planet_plot_small, width=2.5, height=2)







# ***********************************************************************************************
# And again, but with a cloud cover fraction of 90%!
# ***********************************************************************************************
test_cloud_fraction <- 0.90

l89_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                           sample_period == 8, neighborhood_window_width == 30))[100,], 
                                   return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l89_plot <- buildPlot(l89_fit_example)
l89_plot
l89_plot_small <- buildPlotSmall(l89_fit_example)
ggsave(here::here("figures","l89_phenology_retrieval_90_example.png"), l89_plot, width=6, height=4)
ggsave(here::here("figures","l89_phenology_retrieval_90_example_small.png"), l89_plot_small, width=2.5, height=2)


l8_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l8_plot <- buildPlot(l8_fit_example)
l8_plot
l8_plot_small <- buildPlotSmall(l8_fit_example)
ggsave(here::here("figures","l8_phenology_retrieval_90_example.png"), l8_plot, width=6, height=4)
ggsave(here::here("figures","l8_phenology_retrieval_90_example_small.png"), l8_plot_small, width=2.5, height=2)

s2_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 20, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 5, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
s2_plot
s2_plot <- buildPlot(s2_fit_example)
s2_plot
s2_plot_small <- buildPlotSmall(s2_fit_example)
ggsave(here::here("figures","s2_phenology_retrieval_90_example.png"), s2_plot, width=6, height=4)
ggsave(here::here("figures","s2_phenology_retrieval_90_example_small.png"), s2_plot_small, width=2.5, height=2)


l5_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 10, cloudy_fraction == test_cloud_fraction, 
                                                          sample_period == 16, neighborhood_window_width == 30))[100,], 
                                  return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
l5_plot
l5_plot <- buildPlot(l5_fit_example)
l5_plot
l5_plot_small <- buildPlotSmall(l5_fit_example)
ggsave(here::here("figures","l5_phenology_retrieval_90_example.png"), l5_plot, width=6, height=4)
ggsave(here::here("figures","l5_phenology_retrieval_90_example_small.png"), l5_plot_small, width=2.5, height=2)

planet_fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == 5, cloudy_fraction == test_cloud_fraction, 
                                                              sample_period == 2, neighborhood_window_width == 30))[100,], 
                                      return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
planet_plot
planet_plot <- buildPlot(planet_fit_example)
planet_plot
planet_plot_small <- buildPlotSmall(planet_fit_example)
ggsave(here::here("figures","planet_phenology_retrieval_90_example.png"), planet_plot, width=6, height=4)
ggsave(here::here("figures","planet_phenology_retrieval_90_example_small.png"), planet_plot_small, width=2.5, height=2)
