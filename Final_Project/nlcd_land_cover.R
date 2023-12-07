library(raster)
library(terra)

#load in map and locality data
NLCD<-raster ("../../Landcover/nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover_l48_20230630.img")


# Define the UTM Zone 12 CRS
utm_crs <- CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs ")

# Define the extent in UTM Zone 12
utm_extent <- extent(360000, 390000, 4235000, 4265000)
utm_extent <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(utm_extent[1], utm_extent[2], utm_extent[2], utm_extent[1], utm_extent[1]), 
                                                               c(utm_extent[3], utm_extent[3], utm_extent[4], utm_extent[4], utm_extent[3])))), ID = "utm_extent")), proj4string = utm_crs)

# Transform the UTM extent to the current CRS of NLCD
utm_extent <- spTransform(utm_extent, crs(NLCD))

# Crop the raster using the transformed extent
NLCD_cropped <- crop(NLCD, utm_extent)

#plotting classification
plot(NLCD_cropped)


unique(values(NLCD_cropped))


# Assuming NLCD_cropped is your cropped raster layer
points_df <- rasterToPoints(NLCD_cropped)

# Convert to a data frame
df <- as.data.frame(points_df)
df






#setting classes
nlcdclass = c("Not Forest", "Forest", "Forest", "Forest", "Shrubland", "Shrubland", "Barren", "Planted", "Planted", "Wetlands", "Wetlands", "Open Water", "Developed", "Developed", "Developed")

#setting colors
classcolor = c("lightgrey", "green", "green", "green", "brown", "brown", "yellow", "Wh", "yellow", "blue", "blue", "lightblue", "grey", "grey", "grey")


# Your raster layer
nlcd2011 <- NLCD_cropped

# Ratify the raster (define RasterLayer as a categorical variable)
nlcd2011 <- ratify(nlcd2011)
rat <- levels(nlcd2011)[[1]]

# Assign land cover class names and colors to the RAT
rat$landcover <- nlcdclass
rat$color <- classcolor

# Apply the RAT to the raster layer
levels(nlcd2011) <- rat

# Plot the raster layer
plot(nlcd2011, col = rat$color, main = "Classified NLCD Land Cover", legend = FALSE)

# Add a custom legend
legend("topright", legend = rat$landcover, fill = rat$color, title = "Land Cover")







# Get unique values from the raster
unique_values <- unique(values(classified_raster))

# Define RAT
rat <- levels(classified_raster)[[1]]
rat[["LandCover"]] <- data.frame(
  ID = unique_values,
  LandCover = c("Not Forest", "Forest", "Forest", "Forest", "Shrubland", "Shrubland", "Barren", "Planted", "Planted", "Wetlands", "Wetlands", "Open Water", "Developed", "Developed", "Developed"),
  Color = c("lightgrey", "green", "green", "green", "brown", "brown", "yellow", "yellow", "yellow", "blue", "blue", "lightblue", "grey", "grey", "grey")
)

# Ratify the raster
classified_raster <- ratify(classified_raster, rat)

# Plot the classified raster
plot(classified_raster, col = rat$class$Color, main = "Classified NLCD Land Cover", legend = FALSE)

# Add a custom legend
legend("topright", legend = rat$class$LandCover, fill = rat$class$Color, title = "Land Cover")
