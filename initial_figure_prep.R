
library(here)

source(here::here("phenology_fit_tester.R"))

test_output <- apply(parameter_values, getPhenologyFit, MARGIN=1, 
                     return_data_series=FALSE, 
                     try_quadratic=TRUE, 
                     try_linear=TRUE,
                     soil_spec=c(0.24,0.4), 
                     leaf_spec=c(0.05,0.5))
test_df <- bind_rows(test_output)
write_csv(test_df, file=here::here("all_methods_simulation_results.csv"))

# Generate a plot for a given parameter set
getPlot <- function(parameters)
{
  getPhenologyFit(parameters, return_data_series=TRUE)[[9]]
}


# ***************************************************************************************
# *********************** First, let's make a bunch of histograms ***********************
# ***************************************************************************************
# These graphs show a histogram for each combination of sampling period, noise, and cloud cover
# They also have vertical lines superimposed for the mean, the 5th percentile, and 95th percentile
# NOTE - not using these in final paper

set.seed(1)

ggsave(here::here("figures","pheno_fit_goodness_16_days.png"),
       ggplot() +
         geom_histogram(data = test_df %>% filter(sample_period==16, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)),
                        aes(x=r_sqd)) +
         geom_vline(data = test_df %>% filter(sample_period==16, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = median(r_sqd)),
                    aes(xintercept=mean_rsqd),
                    col="red") +
         geom_vline(data = test_df %>% filter(sample_period==16, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = quantile(r_sqd, 0.05)),
                    aes(xintercept=mean_rsqd),
                    col="blue") +
         geom_vline(data = test_df %>% filter(sample_period==16, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = quantile(r_sqd, 0.95)),
                    aes(xintercept=mean_rsqd),
                    col="blue") +
         facet_wrap(~signal_to_noise_ratio+(cloudy_fraction),ncol=4) + 
         scale_y_continuous(breaks=c(0,50,100,150)) + 
         theme(strip.background = element_blank(),
               strip.text.x = element_blank()) + 
         xlab("Frequency") + 
         ylab("R-sqd"),
       width=8, height=9) 

ggsave(here::here("figures","pheno_fit_goodness_8_days.png"),
       ggplot() +
         geom_histogram(data = test_df %>% filter(sample_period==8, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)),
                        aes(x=r_sqd)) +
         geom_vline(data = test_df %>% filter(sample_period==8, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = median(r_sqd)),
                    aes(xintercept=mean_rsqd),
                    col="red") +
         geom_vline(data = test_df %>% filter(sample_period==8, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = quantile(r_sqd, 0.05)),
                    aes(xintercept=mean_rsqd),
                    col="blue") +
         geom_vline(data = test_df %>% filter(sample_period==8, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = quantile(r_sqd, 0.95)),
                    aes(xintercept=mean_rsqd),
                    col="blue") +
         facet_wrap(~signal_to_noise_ratio+(cloudy_fraction),ncol=4) + 
         scale_y_continuous(breaks=c(0,50,100,150)) + 
         theme(strip.background = element_blank(),
               strip.text.x = element_blank()) + 
         xlab("Frequency") + 
         ylab("R-sqd"),
       width=8, height=9) 

ggsave(here::here("figures","pheno_fit_goodness_5_days.png"),
       ggplot() +
         geom_histogram(data = test_df %>% filter(sample_period==5, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)),
                        aes(x=r_sqd)) +
         geom_vline(data = test_df %>% filter(sample_period==5, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = median(r_sqd)),
                    aes(xintercept=mean_rsqd),
                    col="red") +
         geom_vline(data = test_df %>% filter(sample_period==5, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = quantile(r_sqd, 0.05)),
                    aes(xintercept=mean_rsqd),
                    col="blue") +
         geom_vline(data = test_df %>% filter(sample_period==5, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = quantile(r_sqd, 0.95)),
                    aes(xintercept=mean_rsqd),
                    col="blue") +
         facet_wrap(~signal_to_noise_ratio+(cloudy_fraction),ncol=4) + 
         scale_y_continuous(breaks=c(0,50,100,150)) + 
         theme(strip.background = element_blank(),
               strip.text.x = element_blank()) + 
         xlab("Frequency") + 
         ylab("R-sqd"),
       width=8, height=9) 

ggsave(here::here("figures","pheno_fit_goodness_2_days.png"),
       ggplot() +
         geom_histogram(data = test_df %>% filter(sample_period==2, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)),
                        aes(x=r_sqd)) +
         geom_vline(data = test_df %>% filter(sample_period==2, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = median(r_sqd)),
                    aes(xintercept=mean_rsqd),
                    col="red") +
         geom_vline(data = test_df %>% filter(sample_period==2, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = quantile(r_sqd, 0.05)),
                    aes(xintercept=mean_rsqd),
                    col="blue") +
         geom_vline(data = test_df %>% filter(sample_period==2, neighborhood_window_width==30, cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 10, 20, 100, 500)) %>%
                      group_by(cloudy_fraction,signal_to_noise_ratio) %>%
                      summarize(mean_rsqd = quantile(r_sqd, 0.95)),
                    aes(xintercept=mean_rsqd),
                    col="blue") +
         facet_wrap(~signal_to_noise_ratio+(cloudy_fraction),ncol=4) + 
         scale_y_continuous(breaks=c(0,50,100,150)) + 
         theme(strip.background = element_blank(),
               strip.text.x = element_blank()) + 
         xlab("Frequency") + 
         ylab("R-sqd"),
       width=8, height=9) 





# ***************************************************************************************
# ****** Next, we'll do something similar all on one plot, using box plots **************
# ***************************************************************************************
# NOTE - not using these in final paper

boxplot_results <- ggplot(data = test_df %>% 
                            filter(sample_period %in% c(2, 5, 8, 16), 
                                   neighborhood_window_width==30, 
                                   cloudy_fraction %in% c(0, 0.5, 0.75, 0.9), 
                                   signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 20, 100)),
       aes(y=r_sqd, group=sample_period, x=factor(sample_period))) +
  geom_boxplot() + 
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.05,na.rm=T),shape=16, size=2,color="red")+
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.95,na.rm=T),shape=16, size=2,color="blue")+
  geom_hline(yintercept=0.8,linetype='dashed') + 
  facet_wrap(~signal_to_noise_ratio+(cloudy_fraction),ncol=4) + 
  scale_y_continuous(breaks=c(0,50,100,150)) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  xlab("Frequency") + 
  ylab("R-sqd")

ggsave(here::here("figures","boxplot.png"), boxplot_results, width=4, height=7)


# ***************************************************************************************
# *** Next, we'll do something similar AGAIN, but using line plots with quantiles *******
# ***************************************************************************************
# Again, decided not to use this in final paper

lineplot_results <- ggplot(data = test_df %>% 
                             filter(neighborhood_window_width==30, 
                                    cloudy_fraction %in% c(0, 0.25, 0.5, 0.75, 0.9), 
                                    signal_to_noise_ratio %in% c(0.5, 1.0, 5.0, 20, 100)),
                           aes(y=r_sqd, x=sample_period)) +
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.05,na.rm=T),size=1,color="red")+
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.5,na.rm=T),size=1,color="black")+
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.95,na.rm=T),size=1,color="blue")+
  stat_summary(geom = "line", fun = \(x) quantile(x, 0.05,na.rm=T),color="red")+
  stat_summary(geom = "line", fun = \(x) quantile(x, 0.5,na.rm=T),color="black")+
  stat_summary(geom = "line", fun = \(x) quantile(x, 0.95,na.rm=T),color="blue")+
  geom_hline(yintercept=0.8,linetype='dashed') + 
  facet_wrap(~signal_to_noise_ratio+(cloudy_fraction),ncol=4) + 
  scale_y_continuous(breaks=(0:5)/5, limits=c(0,1), expand=c(0,0))+ 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(0.7, "lines")) + 
  xlab("Satellite Return Interval") + 
  ylab("R-sqd")

ggsave(here::here("figures","lineplot.png"), lineplot_results, width=5, height=7)



# *********************************************************************************************************************
# *** Last, let's try saving this while making the X axis the cloud fraction and faceting by sample interval... *******
# *********************************************************************************************************************

lineplot_cf_x <- ggplot(data = test_df %>%
                             filter(neighborhood_window_width==30,
                                    signal_to_noise_ratio %in% c(2.0, 5.0, 20, 100),
                                    fixed_noise == 0.02),
                           aes(y=r_sqd, x=cloudy_fraction)) +
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.05,na.rm=T),size=1,color="red")+
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.5,na.rm=T),size=1,color="black")+
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.95,na.rm=T),size=1,color="blue")+
  stat_summary(geom = "line", fun = \(x) quantile(x, 0.05,na.rm=T),color="red")+
  stat_summary(geom = "line", fun = \(x) quantile(x, 0.5,na.rm=T),color="black")+
  stat_summary(geom = "line", fun = \(x) quantile(x, 0.95,na.rm=T),color="blue")+
  geom_hline(yintercept=0.8,linetype='dashed') +
  facet_wrap(~signal_to_noise_ratio+sample_period,ncol=6) +
  scale_y_continuous(breaks=(1:5)/5, limits=c(0,1), expand=c(0,0))+
  scale_x_continuous(breaks=(1:5)/5, limits=c(0,1), expand=c(0,0))+
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(0.7, "lines")) +
  xlab("Satellite Return Interval") +
  ylab("R-sqd")
ggsave(here::here("figures","lineplot_cf_x.png"), lineplot_cf_x, width=9, height=5)

# This is what we used in final paper!

density_line_plot <- ggplot(data = test_df %>% 
                              filter(neighborhood_window_width==30, 
                                     signal_to_noise_ratio %in% c(2, 5, 20, 100),
                                     fixed_noise == 0.05),
                            aes(y=r_sqd, x=cloudy_fraction)) +
  geom_density_2d_filled(contour_var="ndensity") + 
  stat_summary(geom = "line", fun = \(x) quantile(x, 0.5,na.rm=T),color="red",size=1) +
  geom_hline(yintercept=0.8,linetype='dashed',color="white",size=0.5) + 
  geom_hline(yintercept=0.6,linetype='dashed',color="white",size=0.5) + 
  facet_wrap(~signal_to_noise_ratio+sample_period,ncol=6) + 
  scale_y_continuous(breaks=(1:5)/5, limits=c(0,1), expand=c(0,0))+
  scale_x_continuous(breaks=(1:5)/5, limits=c(0,1), expand=c(0,0))+ 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(0.7, "lines")) + 
  xlab("Satellite Return Interval") + 
  ylab("R-sqd")
density_line_plot
ggsave(here::here("figures","density_line_plot.png"), density_line_plot, width=9, height=5)



test_df_median$method <- "median"
test_df_linear$method <- 'linear'
test_df_full$method <- "quadratic"
test_df_all <- rbind(test_df_median, test_df_linear, test_df_full)


ggsave(here::here("figures","pheno_fit_goodness_1_days_boxplot.png"),
       ggplot(data = test_df_all %>% filter(sample_period==1, 
                                            neighborhood_window_width==30, 
                                            cloudy_fraction %in% c(0, 0.25, 0.5, 0.75, 0.9))) +
         geom_violin(aes(y=r_sqd, x=method, group=factor(method, levels=c("median","linear","quadratic"))), draw_quantiles=c(0.5)) +
         facet_wrap(~cloudy_fraction+signal_to_noise_ratio,ncol=6),
       width=12, height=6) 

ggsave(here::here("figures","pheno_fit_goodness_2_days_boxplot.png"),
       ggplot(data = test_df_all %>% filter(sample_period==2, 
                                            neighborhood_window_width==30, 
                                            cloudy_fraction %in% c(0, 0.25, 0.5, 0.75, 0.9))) +
         geom_violin(aes(y=r_sqd, x=method, group=factor(method, levels=c("median","linear","quadratic"))), draw_quantiles=c(0.5)) +
         facet_wrap(~cloudy_fraction+signal_to_noise_ratio,ncol=6),
       width=12, height=6) 

ggsave(here::here("figures","pheno_fit_goodness_5_days_boxplot.png"),
       ggplot(data = test_df_all %>% filter(sample_period==5, 
                                            neighborhood_window_width==30, 
                                            cloudy_fraction %in% c(0, 0.25, 0.5, 0.75, 0.9))) +
         geom_violin(aes(y=r_sqd, x=method, group=factor(method, levels=c("median","linear","quadratic"))), draw_quantiles=c(0.5)) +
         facet_wrap(~cloudy_fraction+signal_to_noise_ratio,ncol=6),
       width=12, height=6) 

ggsave(here::here("figures","pheno_fit_goodness_8_days_boxplot.png"),
       ggplot(data = test_df_all %>% filter(sample_period==8, 
                                            neighborhood_window_width==30, 
                                            cloudy_fraction %in% c(0, 0.25, 0.5, 0.75, 0.9))) +
         geom_violin(aes(y=r_sqd, x=method, group=factor(method, levels=c("median","linear","quadratic"))), draw_quantiles=c(0.5)) +
         facet_wrap(~cloudy_fraction+signal_to_noise_ratio,ncol=6),
       width=12, height=6) 

ggsave(here::here("figures","pheno_fit_goodness_5_days_boxplot.png"),
       ggplot(data = test_df_all %>% filter(sample_period==5, 
                                            neighborhood_window_width==30, 
                                            cloudy_fraction %in% c(0, 0.25, 0.5, 0.75, 0.9))) +
         geom_violin(aes(y=r_sqd, x=method, group=factor(method, levels=c("median","linear","quadratic"))), draw_quantiles=c(0.5)) +
         facet_wrap(~cloudy_fraction+signal_to_noise_ratio,ncol=6),
       width=12, height=6) 



# Generate a plot for a given parameter set comparing the three methods
getMethodsComparisonPlot <- function(parameters)
{
  # Fit phenoseries with each of three methods
  quadratic_output <- getPhenologyFit(parameters, return_data_series=TRUE, try_quadratic=TRUE, try_linear=TRUE)
  linear_output <- getPhenologyFit(parameters, return_data_series=TRUE, try_quadratic=FALSE, try_linear=TRUE)
  median_output <- getPhenologyFit(parameters, return_data_series=TRUE, try_quadratic=FALSE, try_linear=FALSE)
  
  fitting_points <- quadratic_output[[6]]
  quadratic_fit <- quadratic_output[[7]]
  linear_fit <- linear_output[[7]]
  median_fit <- median_output[[7]]
  random_phenology_reference <- quadratic_output[[8]]
  overpass_doys_cloudless <- quadratic_output[[4]]
  random_phenology_cloudless <- quadratic_output[[5]]
  
  print("Input parameters: ")
  print(parameters[,6:9])
  
  print(paste("Quadratic RMSE: ", quadratic_output[[1]]$rms_error, " and R^2: ", quadratic_output[[1]]$r_sqd, sep=""))
  print(paste("Linear    RMSE: ", linear_output[[1]]$rms_error, " and R^2: ", linear_output[[1]]$r_sqd, sep=""))
  print(paste("Median    RMSE: ", median_output[[1]]$rms_error, " and R^2: ", median_output[[1]]$r_sqd, sep=""))
  
  # Generate output plot comparing phenology prediction to reference
  plot_fit <- ggplot(data=data.frame(dates=fitting_points, 
                                     quadratic=quadratic_fit, 
                                     linear=linear_fit,
                                     median=median_fit,
                                     reference=random_phenology_reference)) + 
    geom_line(aes(x=dates,y=quadratic), col="red", size=1) + 
    geom_line(aes(x=dates,y=linear), col="forestgreen", linetype="dashed", size=1) + 
    geom_line(aes(x=dates,y=median), col="blue", linetype="dotted", size=1) + 
    geom_line(aes(x=dates,y=reference)) + 
    geom_point(data=data.frame(doy=overpass_doys_cloudless, greenness=random_phenology_cloudless),
               aes(x=doy, y=random_phenology_cloudless), col="gray3")
  
  return(plot_fit)
}

method_comparison <- test_df_full 
method_comparison$rms_error_linear <- test_df_linear$rms_error
method_comparison$rms_error_median <- test_df_median$rms_error
method_comparison$r_sqd_linear <- test_df_linear$r_sqd
method_comparison$r_sqd_median <- test_df_median$r_sqd
method_comparison$rms_error_quad_lowest <- (method_comparison$rms_error < method_comparison$rms_error_linear) & (method_comparison$rms_error < method_comparison$rms_error_median)
method_comparison$rms_error_linear_lowest <- (method_comparison$rms_error_linear < method_comparison$rms_error) & (method_comparison$rms_error_linear < method_comparison$rms_error_median)

comparison_summary <- method_comparison %>% 
  filter(neighborhood_window_width == 30,
         cloudy_fraction == 0.5,
         sample_period == 5) %>%
  group_by(signal_to_noise_ratio) %>%
  summarize(quad_lowest = sum(rms_error_quad_lowest) / n(),
            linear_lowest = sum(rms_error_linear_lowest) / n(),
            median_lowest = (n() - sum(rms_error_linear_lowest,rms_error_quad_lowest)) / n(),
            quad_rms = mean(rms_error),
            linear_rms = mean(rms_error_linear),
            median_rms = mean(rms_error_median),
            quad_r_sqd = mean(r_sqd),
            linear_r_sqd = mean(r_sqd_linear),
            median_r_sqd = mean(r_sqd_median))
print(comparison_summary)
ggplot(comparison_summary) + 
  geom_line(aes(x=signal_to_noise_ratio,y=quad_rms), col="red", size=1) + 
  geom_line(aes(x=signal_to_noise_ratio,y=linear_rms), col="forestgreen", linetype="dashed", size=1) + 
  geom_line(aes(x=signal_to_noise_ratio,y=median_rms), col="blue", linetype="dotted", size=1)



comparison_summary <- method_comparison %>% 
  filter(signal_to_noise_ratio == 10,
         neighborhood_window_width == 30) %>%
  group_by(sample_period, cloudy_fraction) %>%
  summarize(quad_lowest = sum(rms_error_quad_lowest) / n(),
            linear_lowest = sum(rms_error_linear_lowest) / n(),
            median_lowest = (n() - sum(rms_error_linear_lowest,rms_error_quad_lowest)) / n(),
            quad_rms = mean(rms_error),
            linear_rms = mean(rms_error_linear),
            median_rms = mean(rms_error_median),
            quad_r_sqd = mean(r_sqd),
            linear_r_sqd = mean(r_sqd_linear),
            median_r_sqd = mean(r_sqd_median))
print(comparison_summary)
ggplot(comparison_summary) + 
  geom_line(aes(x=cloudy_fraction,y=quad_rms,group=sample_period,col=sample_period), size=1) 
ggplot(comparison_summary) + 
  geom_line(aes(x=cloudy_fraction,y=quad_r_sqd,group=sample_period,col=sample_period), size=1) + 
  geom_hline(yintercept=0.9, col="red") + 
  geom_hline(yintercept=0.8, col="pink2")

# # Visualize Landsat Results, 50% clouds
# #  test_df %>% filter(cloudy_fraction == 0.5, sample_period == 5, signal_to_noise_ratio == 20, neighborhood_window_width == 30)
# for(ind in 60841:61141)
# {
#   print(test_output[[ind]][[2]] + geom_text(aes(label=round(test_output[[ind]][[1]]$r_sqd,3), y=0.6, x=0.1)))
#   print(paste("mapped ", ind, sep=""))
#   Sys.sleep(1)
# }
# # Visualize Sentinel-2 Results, 50% clouds
# #  test_df %>% filter(cloudy_fraction == 0.5, sample_period == 5, signal_to_noise_ratio == 20, neighborhood_window_width == 30)
# for(ind in 59221:59371)
# {
#   print(test_output[[ind]][[2]] + geom_text(aes(label=round(test_output[[ind]][[1]]$r_sqd,3), y=0.6, x=0.1)))
#   print(paste("mapped ", ind, sep=""))
#   Sys.sleep(1)
# }


# Visualizing differences by sample window width
#   Try different iterations of the below...
#   With medium noise (e.g. fixed_noise >= 0.05, Landsat TM) 30 performs best 
#   With very high noise (e.g. fixed noise = 0.1, Landsat TIR) 50 to 60 performs best
#   With very low noise, 10 performs best 
ggplot(test %>% filter(signal_to_noise_ratio == 100,
                       fixed_noise == 0.02, 
                       sample_period == 2) %>% 
         group_by(neighborhood_window_width, cloudy_fraction) %>%
         summarize(r_sqd_avg = mean(r_sqd),
                   r_sqd_sd = sd(r_sqd))) + 
  geom_line(aes(y=r_sqd_avg, x=cloudy_fraction, group=neighborhood_window_width, col=as.factor(neighborhood_window_width))) + 
  scale_color_manual(values = c("60" = "red",
                                "50" = "orange",
                                "40" = "yellow",
                                "30" = "green",
                                "20" = "cyan",
                                "10" = "violet"))