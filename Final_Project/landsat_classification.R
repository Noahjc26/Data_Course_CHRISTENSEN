#landsat classification
library(raster)
library(terra)
library(tidyverse)
library(janitor)
library(plotly)
library(cowplot)
library(prismatic)
library(stringr)
library(tmap)
library(mapedit)
library(rpart)
library(rpart.plot)
library(rasterVis)
library(mapview)
library(caret)
library(forcats)
library(tidyterra)
library(e1071)
library(randomForest)

landsat <- stack("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/cropped.tif")

# Plot the raster
plotRGB(landsat, r=4,g=3,b=2, main = "Original RGB",stretch="lin")

# Draw Polygons for Barren/Exposed Rock
barren_polygons_1 <- drawPoly()
barren_polygons_2 <- drawPoly()

# Draw Polygons for snow
snow_polygons_1 <- drawPoly()
snow_polygons_2 <- drawPoly()

# Draw Polygons for vegetation
vegetation_polygons_1 <- drawPoly()
vegetation_polygons_2 <- drawPoly()

# Combine Barren Polygons into a single SpatialPolygons object
barren_sp <- SpatialPolygons(
  list(
    Polygons(list(Polygon(barren_polygons_1)), ID = 1),
    Polygons(list(Polygon(barren_polygons_2)), ID = 2)
  )
)

# Create a data.frame with Class information
barren_data <- data.frame(Class = c("Barren","Barren"), row.names = c("1", "2"))

# Create a SpatialPolygonsDataFrame for Barren
barren_spdf <- SpatialPolygonsDataFrame(
  barren_sp,
  data = barren_data,
  match.ID = FALSE
)

# Combine Snow Polygons into a single SpatialPolygonsDataFrame
snow_sp <- SpatialPolygons(
  list(
    Polygons(list(Polygon(snow_polygons_1)), ID = 1),
    Polygons(list(Polygon(snow_polygons_2)), ID = 2)
  )
)

# Create a data.frame with Class information
snow_data <- data.frame(Class = c("Snow","Snow"), row.names = c("1", "2"))

# Create a SpatialPolygonsDataFrame for Barren
snow_spdf <- SpatialPolygonsDataFrame(
  snow_sp,
  data = snow_data,
  match.ID = FALSE
)

# Combine Vegetation Polygons into a single SpatialPolygonsDataFrame
vegetation_sp <- SpatialPolygons(
  list(
    Polygons(list(Polygon(vegetation_polygons_1)), ID = 1),
    Polygons(list(Polygon(vegetation_polygons_2)), ID = 2)
  )
)

# Create a data.frame with Class information
vegetation_data <- data.frame(Class = c("Vegetation","Vegetation"), row.names = c("1", "2"))

# Create a SpatialPolygonsDataFrame for Barren
vegetation_spdf <- SpatialPolygonsDataFrame(
  vegetation_sp,
  data = vegetation_data,
  match.ID = FALSE
)



# Create an empty data frame to store training data
all_training_data <- data.frame()

# Loop through each class and its corresponding polygons
classes_spdf <- list(barren_spdf, snow_spdf, vegetation_spdf)
for (class_spdf in classes_spdf) {
  # Extract pixel values for the current class
  class_training_data <- terra::extract(landsat, class_spdf)
  
  # Combine pixel values with class labels
  class_df <- data.frame(matrix(unlist(class_training_data), ncol = nlayers(landsat), byrow = TRUE))
  colnames(class_df) <- paste0("Band", 1:nlayers(landsat))
  class_df$Class <- as.factor(class_spdf$Class)
  
  # Append the current class data to the overall training data
  all_training_data <- rbind(all_training_data, class_df)
}


# Train the Random Forest model (or any other classifier of your choice)
rf_model <- randomForest(Class ~ ., data = all_training_data)

# Assuming your Landsat data is a raster stack named 'landsat'
landsat_values <- terra::extract(landsat, raster::extent(landsat))

# Convert to a data frame
landsat_df <- data.frame(matrix(unlist(landsat_values), ncol = nlayers(landsat), byrow = TRUE))
colnames(landsat_df) <- paste0("Band", 1:nlayers(landsat))

# If your model was trained using the 'randomForest' package
prediction_probs <- predict(rf_model, landsat_df, type = "response")

# Find the predicted class based on the highest probability
predicted_class <- colnames(prediction_probs)[apply(prediction_probs, 1, which.max)]

# Check unique values in the predicted_class vector
unique_values <- unique(predicted_class)
print(unique_values)

# Plot the predicted class map
plot(predicted_class, main = "Predicted Class Map")

