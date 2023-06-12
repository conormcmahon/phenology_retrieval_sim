
library(raster)
library(here)
library(sf)
library(tidyverse)
library(psych)

# Load class raster data output by classifier
spr_classes <- raster("D:/SERDP/Huachuca/Riparian_Zone/random_forest/classified_full.tif")
# Load validation polygons, transform to same CRS
validation <- st_read("D:/SERDP/Huachuca/vegetation_maps/spr_aerial_final_interpretation.gpkg")
validation <- st_transform(validation, st_crs(spr_classes))
# Check how much area there is under each class for balance
validation$area <- st_area(validation)
validation %>% group_by(class) %>% summarize(total_area = sum(area))

# Convert validation class strings to numeric format
classes <- c(1, 2, 3, 4)
names(classes) <- c("cottonwood", "mesquite", "grassland", "soil")
validation$class_num <- classes[validation$class]

# Function to extract class data from under a validation polygon
getDataAtPolygon <- function(index, img)
{
  point <- validation[index,]
  print("Working on the following point: ")
  print(point)
  new_data <- as.data.frame(raster::extract(img, point))
  names(new_data) <- "predicted"
  new_data$actual <- point$class_num
  new_data$point_id <- point$point_id
  
  print("")
  return(new_data)
}
# Get all class / validation data
predictions <- bind_rows(lapply(1:nrow(validation), getDataAtPolygon, img=spr_classes))

predictions %>% group_by(predicted, actual) %>% tally()

cohen.kappa(x=cbind(predictions$predicted, predictions$actual))
