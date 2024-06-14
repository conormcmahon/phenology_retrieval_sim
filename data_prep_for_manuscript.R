
library(tidyverse)
library(tictoc)
library(zoo)
library(ggtext)

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
    geom_text(aes(x=325, y=0.75, label=as.numeric(round(input[[1]]["r_sqd"],3)), hjust="left")) + 
    geom_text(aes(x=325, y=0.65, label=as.numeric(round(input[[1]]["rms_error"],3)), hjust="left")) + 
    scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
    scale_x_continuous(expand=c(0,0))
  return(plot)
}
buildPlotSmall <- function(input)
{
  plot <- input[[9]] + 
    #geom_text(aes(x=250, y=1.25, label=as.numeric(round(input[[1]]["r_sqd"],4)), hjust="left")) + 
    #geom_text(aes(x=250, y=-0.3, label=as.numeric(round(input[[1]]["rms_error"],4)), hjust="left")) + 
    geom_richtext(aes(label = paste("R<sup>2</sup> = ", 
                                    round(as.numeric(input[[1]]["r_sqd"]), 3), sep=""), 
                      x = 350, y = 1.25, label.size=NA), 
                  hjust='right', size=5, fill=scales::alpha("lightgray",0.6)) + 
    geom_richtext(aes(label = paste("RMSE = ", 
                                    round(as.numeric(input[[1]]["rms_error"]), 3), sep=""), 
                      x = 350, y = -0.3, label.size=NA), 
                  hjust='right', size=5, fill=scales::alpha("lightgray",0.6)) + 
    scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) + 
    scale_x_continuous(expand=c(0,0)) + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  return(plot)
}
  
generateAndSavePlot <- function(sensor_name,
                                cloudy_fraction_target,
                                signal_to_noise_ratio_target, 
                                sample_period_target,
                                neighborhood_window_width_target,
                                fixed_noise_target)
{
  fit_example <- getPhenologyFit((test_df %>% filter(signal_to_noise_ratio == signal_to_noise_ratio_target, cloudy_fraction == cloudy_fraction_target, 
                                                     sample_period == sample_period_target, neighborhood_window_width == neighborhood_window_width_target, 
                                                     fixed_noise == fixed_noise_target))[101,], 
                                     return_data_series=TRUE, try_quadratic=TRUE, try_linear=TRUE)
  new_plot <- buildPlot(fit_example)
  new_plot_small <- buildPlotSmall(fit_example)
  new_plot_small
  ggsave(here::here("figures",paste(sensor_name,"_phenology_retrieval_",100*cloudy_fraction_target,"_example.png",sep="")), new_plot, width=6, height=4)
  ggsave(here::here("figures",paste(sensor_name,"_phenology_retrieval_",100*cloudy_fraction_target,"_example_small.png",sep="")), new_plot_small, width=2.5, height=2)
}

# ***********************************************************************************************
# First off, lets print some example plots of phenology fits for given datasets
#   FIRST with no cloud cover
# ***********************************************************************************************

set.seed(1)

test_cloud_fraction <- 0

generateAndSavePlot("modis",  test_cloud_fraction, 100, 1,  30, 0.02)
generateAndSavePlot("planet", test_cloud_fraction, 20,  2,  30, 0.05)
generateAndSavePlot("s2",     test_cloud_fraction, 100, 5,  30, 0.02)
generateAndSavePlot("l89",    test_cloud_fraction, 100, 8,  30, 0.02)
generateAndSavePlot("l8",     test_cloud_fraction, 100, 16, 30, 0.02)
generateAndSavePlot("l5",     test_cloud_fraction, 20,  16, 30, 0.05)

# ***********************************************************************************************
# NOW let's do the same, but with a cloud cover fraction of 50%!
# ***********************************************************************************************

test_cloud_fraction <- 0.5

generateAndSavePlot("modis",  test_cloud_fraction, 100, 1,  30, 0.02)
generateAndSavePlot("planet", test_cloud_fraction, 20,  2,  30, 0.05)
generateAndSavePlot("s2",     test_cloud_fraction, 100, 5,  30, 0.02)
generateAndSavePlot("l89",    test_cloud_fraction, 100, 8,  30, 0.02)
generateAndSavePlot("l8",     test_cloud_fraction, 100, 16, 30, 0.02)
generateAndSavePlot("l5",     test_cloud_fraction, 20,  16, 30, 0.05)


# ***********************************************************************************************
# And again, but with a cloud cover fraction of 75%!
# ***********************************************************************************************

test_cloud_fraction <- 0.75

generateAndSavePlot("modis",  test_cloud_fraction, 100, 1,  30, 0.02)
generateAndSavePlot("planet", test_cloud_fraction, 20,  2,  30, 0.05)
generateAndSavePlot("s2",     test_cloud_fraction, 100, 5,  30, 0.02)
generateAndSavePlot("l89",    test_cloud_fraction, 100, 8,  30, 0.02)
generateAndSavePlot("l8",     test_cloud_fraction, 100, 16, 30, 0.02)
generateAndSavePlot("l5",     test_cloud_fraction, 20,  16, 30, 0.05)

# ***********************************************************************************************
# And again, but with a cloud cover fraction of 90%!
# ***********************************************************************************************

test_cloud_fraction <- 0.90

generateAndSavePlot("modis",  test_cloud_fraction, 100, 1,  30, 0.02)
generateAndSavePlot("planet", test_cloud_fraction, 20,  2,  30, 0.05)
generateAndSavePlot("s2",     test_cloud_fraction, 100, 5,  30, 0.02)
generateAndSavePlot("l89",    test_cloud_fraction, 100, 8,  30, 0.02)
generateAndSavePlot("l8",     test_cloud_fraction, 100, 16, 30, 0.02)
generateAndSavePlot("l5",     test_cloud_fraction, 20,  16, 30, 0.05)

