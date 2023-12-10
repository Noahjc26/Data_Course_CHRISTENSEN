library(raster)
library(terra)
library(rasterVis)

#load in map and locality data
NLCD<-raster("../../Landcover/nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover_l48_20230630.img")

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


# Create a new raster with the same attributes as NLCD_cropped
reclassed_raster <- NLCD_cropped

# Set values below 24 to 1
reclassed_raster[values(reclassed_raster) < 20] <- 2

# Set values between 24 and 90 to 2
reclassed_raster[values(reclassed_raster) >= 20 & values(reclassed_raster) <= 40] <- 1

# Set values between 24 and 90 to 2
reclassed_raster[values(reclassed_raster) >50] <- 1

# Set values above 90 to 3
reclassed_raster[values(reclassed_raster) >= 40 & values(reclassed_raster) <= 50] <- 3

values(reclassed_raster)


# Plot the new raster
levelplot(reclassed_raster, main = "Classified NLCD Land Cover", legend = TRUE)


#reading in classified raster
ls <- raster("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/classification.tif")

plot(ls)

# Project ls to have the same extent and resolution as reclassed_raster, using method nearest neighbor"
ls_projected <- projectRaster(ls, reclassed_raster,method = "ngb")


plot(ls_projected)


# Assuming reclassed_raster and ls are your rasters
reference <- values(reclassed_raster)
predicted <- values(ls_projected)


# Assuming predicted and reference are your vectors
predicted <- as.factor(predicted)
reference <- as.factor(reference)

# Create a confusion matrix
conf_matrix <- confusionMatrix(predicted, reference)

# Print the confusion matrix
print(conf_matrix)

# Extract accuracy and other metrics
accuracy <- conf_matrix$overall["Accuracy"]
kappa <- conf_matrix$overall["Kappa"]

# Print accuracy and kappa
cat("Accuracy:", accuracy, "\n")
cat("Kappa:", kappa, "\n")



# Create a custom legend
legend_labels <- c("Barren", "Snow", "Vegetation")
legend_colors <- c("white", "yellow", "green")

# Plot the raster using levelplot
levelplot(ls, col.regions = legend_colors, at = c(0.5, 1.5, 2.5, 3.5),
          margin = FALSE, main = "Landsat Cart classification Land Cover",
          colorkey = list(labels = list(at = 1:3, labels = legend_labels)))

# Add the legend manually
legend("bottomright", legend = legend_labels, fill = legend_colors, title = "Land Cover")

