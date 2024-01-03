library(terra)
aster <- rast("../../ASTER/2005_10_01/full_cleaned.tif")

plotRGB(aster,4,2,1,stretch="lin")


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

#making sure its in raster format for the loop
aster <- stack(aster)

# Loop through each class and its corresponding polygons
classes_spdf <- list(barren_spdf, snow_spdf, vegetation_spdf)
for (class_spdf in classes_spdf) {
  # Extract pixel values for the current class
  class_training_data <- terra::extract(aster, class_spdf)
  
  # Combine pixel values with class labels
  class_df <- data.frame(matrix(unlist(class_training_data), ncol = nlayers(aster), byrow = TRUE))
  colnames(class_df) <- paste0("Band", 1:nlayers(aster))
  class_df$Class <- as.factor(class_spdf$Class)
  
  # Append the current class data to the overall training data
  all_training_data <- rbind(all_training_data, class_df)
}

saveRDS(all_training_data,"./polygon_training_data_aster.rds")
