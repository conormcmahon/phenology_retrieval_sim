
library(tidyverse)
library(raster)
library(sf)
library(ggtext)

# This loads three co-located phenology images from Landsat, Sentinel-2, and MODIS 
# Then it subsamples a set of polygons within each which correspond to one MODIS pixel
# For each polygon it averages values for the many subtended Landsat and Sentinel-2 pixels
# Then it compares the MODIS values to the averaged Landsat/Sentinel-2 values

# First, to load the imagery
# They are 52-band imagery where each band is greenness in a given week 
sen2_img <- stack(here::here("..","sensor_comparison","san_antonio_creek_s2_2022_phenoseries.tif"))
landsat_img <- stack(here::here("..","sensor_comparison","san_antonio_creek_landsat_2022_phenoseries.tif"))
modis_img <- stack(here::here("..","sensor_comparison","san_antonio_creek_modis_2022_phenoseries.tif"))

# Then, load sample points
# There are 14 samples from these classes: willow, grassland, chaparral, bare, golf_course
# The golf course class is more structurally complex, with a mix of turfgrass and non-native evergreen trees
sample_points <- st_read(here::here("..","sensor_comparison","san_antonio_creek_points.gpkg"))
sample_points$point_id <- 1:nrow(sample_points)

getDataAtPolygon <- function(point, img)
{
  print("Working on the following point: ")
  print(point)
  new_data <- as.data.frame(raster::extract(img, point))
  names(new_data) <- paste("week_", 1:52, sep="")
  new_data$class <- point$class
  new_data$point_id <- point$point_id
  
  print("")
  return(new_data)
}

# Sample polygons within each image
sen2_data <- bind_rows(lapply(1:nrow(sample_points), function(ind) { getDataAtPolygon(sample_points[ind,], sen2_img) } ))
landsat_data <- bind_rows(lapply(1:nrow(sample_points), function(ind) { getDataAtPolygon(sample_points[ind,], landsat_img) } ))
modis_data <- bind_rows(lapply(1:nrow(sample_points), function(ind) { getDataAtPolygon(sample_points[ind,], modis_img) } ))

# Pivot to a long format (instead of each week being a band)
sen2_long <- sen2_data %>% 
  pivot_longer(1:52, names_to="week_str", values_to="ndvi") %>% 
  mutate(week = as.numeric(substr(week_str, 6,10)))
landsat_long <- landsat_data %>% 
  pivot_longer(1:52, names_to="week_str", values_to="ndvi") %>% 
  mutate(week = as.numeric(substr(week_str, 6,10)))
modis_long <- modis_data %>% 
  pivot_longer(1:52, names_to="week_str", values_to="ndvi") %>% 
  mutate(week = as.numeric(substr(week_str, 6,10)))


# Get average dataframes for Sentinel-2 and Landsat within the MODIS pixels
sen2_avg <- sen2_long %>% 
  group_by(week, point_id) %>%
  summarize(mean_sen2 = mean(ndvi), 
            q05_sen2 = quantile(ndvi, 0.05), 
            q50_sen2 = quantile(ndvi, 0.5), 
            q95_sen2 = quantile(ndvi, 0.95))
landsat_avg <- landsat_long %>% 
  group_by(week, point_id) %>%
  summarize(mean_landsat = mean(ndvi), 
            q05_landsat = quantile(ndvi, 0.05), 
            q50_landsat = quantile(ndvi, 0.5), 
            q95_landsat = quantile(ndvi, 0.95))

all_data <- merge(modis_long, sen2_avg)
all_data <- merge(all_data, landsat_avg)
names(all_data) <- c("point_id", "week", "class", "week_str", "ndvi_modis", "ndvi_sen2", "ndvi_sen2_q05", "ndvi_sen2_q50", "ndvi_sen2_95", "ndvi_landsat", "q05_landsat", "q50_landsat", "q95_landsat")

# Simple root mean square error between two vectors
rootMeanSquareError <- function(vec_1, vec_2)
{
  sqrt(mean((vec_1 - vec_2)^2))
}

# OLS Bisector Method
#   Gets the bisecting angle between both y = f(x) and x = f(y)
#   This method is recommended for cases where both x and y are observed variables which contain measurement error 


# Compares the Sentinel-2, Landsat, and MODIS timeseries for a given sample 
getRSqdData <- function(ind){
  # Subset to current target point index (ind)
  current_subset <- all_data %>% filter(point_id == ind)
  # Generate linear models comparing the phenology timeseries
  modis_landsat <- summary(lm(data=current_subset, ndvi_modis ~ ndvi_landsat))
  modis_sen2 <- summary(lm(data=current_subset, ndvi_modis ~ ndvi_sen2))
  landsat_sen2 <- summary(lm(data=current_subset, ndvi_landsat ~ ndvi_sen2))
  
  return(
    data.frame(ml_rsqd = modis_landsat$r.squared,
               ml_int = modis_landsat$coefficients[1,1],
               ml_slope = modis_landsat$coefficients[2,1],
               ml_rmse = rootMeanSquareError(current_subset$ndvi_modis, current_subset$ndvi_landsat),
               ms_rsqd = modis_sen2$r.squared,
               ms_int = modis_sen2$coefficients[1,1],
               ms_slope = modis_sen2$coefficients[2,1],
               ms_rmse = rootMeanSquareError(current_subset$ndvi_modis, current_subset$ndvi_sen2),
               ls_rsqd = landsat_sen2$r.squared,
               ls_int = landsat_sen2$coefficients[1,1],
               ls_slope = landsat_sen2$coefficients[2,1],
               ls_rmse = rootMeanSquareError(current_subset$ndvi_landsat, current_subset$ndvi_sen2),
               point_id = ind)
  )
}

model_comparison <- bind_rows(lapply(1:nrow(sample_points), getRSqdData))

# Generate some plots for visual comparison
phenology_comparison <- ggplot() + 
  geom_line(data = all_data,
            aes(x=week,y=ndvi_sen2), 
            col="red") + 
  geom_line(data = all_data,
            aes(x=week,y=ndvi_landsat), 
            col="orange2") + 
  geom_line(data = all_data,
            aes(x=week,y=ndvi_modis),
            col="blue") + 
  geom_text(data = model_comparison, 
            aes(label = round(ml_rsqd, 3), x = 40, y = 0.42), hjust='left') + 
  geom_text(data = model_comparison, 
            aes(label = round(ms_rsqd, 3), x = 40, y = 0.3), hjust='left') + 
  geom_text(data = model_comparison, 
            aes(label = round(ls_rsqd, 3), x = 40, y = 0.18), hjust='left') + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  scale_y_continuous(limits=c(0,1.0), breaks=(1:5)/5, expand=c(0,0)) + 
  scale_x_continuous(limits=c(0,52), breaks=(1:5)*10, expand=c(0,0)) + 
  facet_wrap(~point_id) + 
  xlab("Week") + 
  ylab("Normalized Difference Vegetation Index")
ggsave(here::here("figures","sensor_comparison.png"), phenology_comparison, width=8, height=5)

# Generate means for each vegetation class 
all_data_by_class <- all_data %>%
  group_by(class, week) %>% 
  summarize(ndvi_sen2 = mean(ndvi_sen2),
            ndvi_landsat = mean(ndvi_landsat),
            ndvi_modis = mean(ndvi_modis))
model_comparison$class <- sample_points$class
model_comparison_by_class <- model_comparison %>%
  group_by(class) %>% 
  summarize(ml_rsqd = mean(ml_rsqd),
            ml_int = mean(ml_int),
            ml_slope = mean(ml_slope),
            ml_rmse = mean(ml_rmse),
            ms_rsqd = mean(ms_rsqd),
            ms_int = mean(ms_int),
            ms_slope = mean(ms_slope),
            ms_rmse = mean(ms_rmse),
            ls_rsqd = mean(ls_rsqd),
            ls_int = mean(ls_int),
            ls_slope = mean(ls_slope),
            ls_rmse = mean(ls_rmse))

# Generate some plots for visual comparison
phenology_comparison <- ggplot() + 
  geom_line(data = all_data,
            aes(x=week,y=ndvi_sen2), 
            col="red") + 
  geom_line(data = all_data,
            aes(x=week,y=ndvi_landsat), 
            col="orange2") + 
  geom_line(data = all_data,
            aes(x=week,y=ndvi_modis),
            col="blue") + 
  geom_text(data = model_comparison, 
            aes(label = round(ml_rsqd, 3), x = 40, y = 0.42), hjust='left') + 
  geom_text(data = model_comparison, 
            aes(label = round(ms_rsqd, 3), x = 40, y = 0.3), hjust='left') + 
  geom_text(data = model_comparison, 
            aes(label = round(ls_rsqd, 3), x = 40, y = 0.18), hjust='left') + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  scale_y_continuous(limits=c(0,1.0), breaks=(1:5)/5, expand=c(0,0)) + 
  scale_x_continuous(limits=c(0,52), breaks=(1:5)*10, expand=c(0,0)) + 
  facet_wrap(~point_id) + 
  xlab("Week") + 
  ylab("Normalized Difference Vegetation Index")
ggsave(here::here("figures","sensor_comparison.png"), phenology_comparison, width=8, height=5)

# Generate some plots for visual comparison
# Similar to the above, but by vegetation class
phenology_comparison <- ggplot() + 
  geom_line(data = all_data_by_class %>% filter(class != "golf_course"),
            aes(x=week,y=ndvi_sen2), 
            col="red") + 
  geom_line(data = all_data_by_class %>% filter(class != "golf_course"),
            aes(x=week,y=ndvi_landsat), 
            col="orange2") + 
  geom_line(data = all_data_by_class %>% filter(class != "golf_course"),
            aes(x=week,y=ndvi_modis),
            col="blue") + 
  geom_richtext(data = data.frame(temp = 1),
                aes(label = "<b>r<br>RMSE</b>"), x = 7, y = 0.92, 
                label.colour="white") + 
  geom_richtext(data = model_comparison_by_class %>% filter(class != "golf_course"), 
            aes(label = paste("<b>M / L</b><br>", round(ml_rsqd^.5, 3), "<br>", round(ml_rmse, 3), sep=""), x = 13, y = 0.96), 
            hjust='left', label.colour="white") + 
  geom_richtext(data = model_comparison_by_class %>% filter(class != "golf_course"), 
            aes(label = paste("<b>M / S</b><br>", round(ms_rsqd^.5, 3), "<br>", round(ms_rmse, 3), sep=""), x = 26, y = 0.96), 
            hjust='left', label.colour="white") + 
  geom_richtext(data = model_comparison_by_class %>% filter(class != "golf_course"), 
            aes(label = paste("<b>L / S</b><br>", round(ls_rsqd^.5, 3), "<br>", round(ls_rmse, 3), sep=""), x = 39, y = 0.96), 
            hjust='left',label.colour="white") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  scale_y_continuous(limits=c(0,1.1), breaks=(0:5)/5, expand=c(0,0)) + 
  scale_x_continuous(limits=c(0,52), breaks=(1:5)*10, expand=c(0,0)) + 
  facet_wrap(~factor(class, 
                     levels=c("willow","chaparral","grassland","golf_course","sand")),
             nrow=1) + 
  xlab("Week") + 
  ylab("NDVI")
ggsave(here::here("figures","sensor_comparison_by_class.png"), phenology_comparison, width=8, height=3)


# Compares the Sentinel-2, Landsat, and MODIS timeseries for a given class 
getRSqdDataClasswise <- function(class_current){
  # Subset to current target point index (ind)
  current_subset <- all_data_by_class %>% filter(class == class_current)
  # Generate linear models comparing the phenology timeseries
  modis_landsat <- summary(lm(data=current_subset, ndvi_modis ~ ndvi_landsat))
  modis_sen2 <- summary(lm(data=current_subset, ndvi_modis ~ ndvi_sen2))
  landsat_sen2 <- summary(lm(data=current_subset, ndvi_landsat ~ ndvi_sen2))
  
  return(
    data.frame(ml_rsqd = modis_landsat$r.squared,
               ml_int = modis_landsat$coefficients[1,1],
               ml_slope = modis_landsat$coefficients[2,1],
               ml_rmse = rootMeanSquareError(current_subset$ndvi_modis, current_subset$ndvi_landsat),
               ms_rsqd = modis_sen2$r.squared,
               ms_int = modis_sen2$coefficients[1,1],
               ms_slope = modis_sen2$coefficients[2,1],
               ml_rmse = rootMeanSquareError(current_subset$ndvi_modis, current_subset$ndvi_sen2),
               ls_rsqd = landsat_sen2$r.squared,
               ls_int = landsat_sen2$coefficients[1,1],
               ls_slope = landsat_sen2$coefficients[2,1],
               ml_rmse = rootMeanSquareError(current_subset$ndvi_landsat, current_subset$ndvi_sen2),
               class = class_current)
  )
}
model_comparison_classwise <- bind_rows(lapply(unique(all_data_by_class$class), getRSqdDataClasswise))

ml_model_overall <- lm(data = all_data, ndvi_modis ~ ndvi_landsat)
ml_rsqd_overall <- summary(ml_model_overall)$r.squared
ml_int_overall <- summary(ml_model_overall)$coefficient[1,1]
ml_slope_overall <- summary(ml_model_overall)$coefficient[2,1]
ml_rmse_overall <- rootMeanSquareError(all_data$ndvi_modis, all_data$ndvi_landsat)

ms_model_overall <- lm(data = all_data, ndvi_modis ~ ndvi_sen2)
ms_rsqd_overall <- summary(ms_model_overall)$r.squared
ms_int_overall <- summary(ms_model_overall)$coefficient[1,1]
ms_slope_overall <- summary(ms_model_overall)$coefficient[2,1]
ms_rmse_overall <- rootMeanSquareError(all_data$ndvi_modis, all_data$ndvi_sen2)

ls_model_overall <- lm(data = all_data, ndvi_landsat ~ ndvi_sen2)
ls_rsqd_overall <- summary(ls_model_overall)$r.squared
ls_int_overall <- summary(ls_model_overall)$coefficient[1,1]
ls_slope_overall <- summary(ls_model_overall)$coefficient[2,1]
ls_rmse_overall <- rootMeanSquareError(all_data$ndvi_landsat, all_data$ndvi_sen2)


# Sensor to Sensor Comparisons
labelsize <- 3
modis_landsat_comparison <- ggplot(all_data) + 
  geom_point(aes(x=ndvi_modis, y=ndvi_landsat, col=factor(week))) + 
  geom_abline(slope = 1, intercept = 0) +
  xlab("MODIS NDVI") + 
  ylab("Landsat NDVI")+ 
  theme_bw() + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) + 
 # geom_richtext(aes(label = paste("R<sup>2</sup> = ", round(ml_rsqd_overall, 3), sep=""), x = 0.47, y = 0.95), hjust='right', label.colour="white", size=labelsize) + 
  geom_richtext(aes(label = paste("r = ", round(ml_rsqd_overall^0.5, 3), sep=""), x = 0.47, y = 0.95), hjust='right', label.colour="white", size=labelsize) + 
  geom_richtext(aes(label = paste("RMSE = ", round(ml_rmse_overall, 3), sep=""), x = 0.47, y = 0.85), hjust='right', label.colour="white", size=labelsize) + 
 # geom_richtext(aes(label = paste("b = ", round(ml_slope_overall, 3), sep=""), x = 0.47, y = 0.75), hjust='right', label.colour="white", size=labelsize) + 
 # geom_richtext(aes(label = paste("m = ", round(ml_int_overall, 3), sep=""), x = 0.47, y = 0.65), hjust='right', label.colour="white", size=labelsize) + 
  scale_color_viridis_d() 
ggsave(here::here("figures","modis_landsat_comparison.png"), modis_landsat_comparison, width=2.5, height=2.5)
modis_sen2_comparison <- ggplot(all_data) + 
  geom_point(aes(x=ndvi_modis, y=ndvi_sen2, col=factor(week))) + 
  geom_abline(slope = 1, intercept = 0) +
  xlab("MODIS NDVI") + 
  ylab("Sentinel-2 NDVI")+ 
  theme_bw() + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) + 
  # geom_richtext(aes(label = paste("R<sup>2</sup> = ", round(ml_rsqd_overall, 3), sep=""), x = 0.47, y = 0.95), hjust='right', label.colour="white", size=labelsize) + 
  geom_richtext(aes(label = paste("r = ", round(ms_rsqd_overall^0.5, 3), sep=""), x = 0.47, y = 0.95), hjust='right', label.colour="white", size=labelsize) + 
  geom_richtext(aes(label = paste("RMSE = ", round(ms_rmse_overall, 3), sep=""), x = 0.47, y = 0.85), hjust='right', label.colour="white", size=labelsize) + 
  # geom_richtext(aes(label = paste("b = ", round(ms_slope_overall, 3), sep=""), x = 0.47, y = 0.75), hjust='right', label.colour="white", size=labelsize) + 
  # geom_richtext(aes(label = paste("m = ", round(ms_int_overall, 3), sep=""), x = 0.47, y = 0.65), hjust='right', label.colour="white", size=labelsize) + 
  scale_color_viridis_d() 
ggsave(here::here("figures","modis_sen2_comparison.png"), modis_sen2_comparison, width=2.5, height=2.5)
landsat_sentinel_comparison <- ggplot(all_data) + 
  geom_point(aes(x=ndvi_landsat, y=ndvi_sen2, col=factor(week))) + 
  geom_abline(slope = 1, intercept = 0) +
  xlab("Landsat NDVI") + 
  ylab("Sentinel-2 NDVI")+ 
  theme_bw() + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  # geom_richtext(aes(label = paste("R<sup>2</sup> = ", round(ls_rsqd_overall, 3), sep=""), x = 0.47, y = 0.95), hjust='right', label.colour="white", size=labelsize) + 
  geom_richtext(aes(label = paste("r = ", round(ls_rsqd_overall^0.5, 3), sep=""), x = 0.47, y = 0.95), hjust='right', label.colour="white", size=labelsize) + 
  geom_richtext(aes(label = paste("RMSE = ", round(ls_rmse_overall, 3), sep=""), x = 0.47, y = 0.85), hjust='right', label.colour="white", size=labelsize) + 
  # geom_richtext(aes(label = paste("b = ", round(ls_slope_overall, 3), sep=""), x = 0.47, y = 0.75), hjust='right', label.colour="white", size=labelsize) + 
  # geom_richtext(aes(label = paste("m = ", round(ls_int_overall, 3), sep=""), x = 0.47, y = 0.65), hjust='right', label.colour="white", size=labelsize) + 
  scale_color_viridis_d() 
ggsave(here::here("figures","landsat_sentinel_comparison.png"), landsat_sentinel_comparison, width=2.5, height=2.5)
modis_landsat_comparison <- ggplot(all_data) + 
  geom_point(aes(x=ndvi_landsat, y=ndvi_sen2, col=factor(week))) +  
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() + 
  scale_color_viridis_d() 
ggsave(here::here("figures","comparison_legend.png"), modis_landsat_comparison, width=2.5, height=2.5)




# Direct pixel-by-pixel comparison of Sen2 and Landsat imagery, aggregated up to 30 m
sen2_ag <- terra::aggregate(sen2_img, 3)
landsat_ag <- terra::aggregate(landsat_img, 3)

sen2_ag_long <- as.data.frame(getValues(sen2_ag)) %>% 
  pivot_longer(cols=1:52, names_to="week_var_string", values_to="ndvi") %>%
  mutate(nchar_var_string=nchar(week_var_string),
         week=substr(week_var_string,2,(nchar_var_string-30)),
         sensor="Sentinel-2")
landsat_ag_long <- as.data.frame(getValues(landsat_ag)) %>% 
  pivot_longer(cols=1:52, names_to="week_var_string", values_to="ndvi") %>%
  mutate(nchar_var_string=nchar(week_var_string),
         week=substr(week_var_string,2,(nchar_var_string-30)),
         sensor="Landsat")
long_ag_df <- sen2_ag_long[,4]
long_ag_df$ndvi_s2 <- sen2_ag_long$ndvi
long_ag_df$ndvi_l <- landsat_ag_long$ndvi
# Visualize comparison of ALL Landsat vs Sen2 phenology data
ggplot(long_ag_df) + 
  geom_density_2d_filled(aes(x=ndvi_s2,
                             y=ndvi_l),
                         contour_var = "ndensity")
# Get linear model again of ALL Landsat vs. Sen2 phenology data
summary(lm(data=long_ag_df, ndvi_s2~ndvi_l))
# Call:
#   lm(formula = ndvi_s2 ~ ndvi_l, data = long_ag_df)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.91983 -0.02785 -0.00008  0.02619  0.72218 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -4.711e-02  6.625e-05    -711   <2e-16 ***
#   ndvi_l       1.031e+00  1.519e-04    6788   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0469 on 2856722 degrees of freedom
# Multiple R-squared:  0.9416,	Adjusted R-squared:  0.9416 
# F-statistic: 4.608e+07 on 1 and 2856722 DF,  p-value: < 2.2e-16


