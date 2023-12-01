#PCA
library(rasterVis)
library(raster)
library(tidyverse)
library(viridis)
library(gridExtra)
library(shiny)
library(shinyjs)
library(imager)


band1 <- raster("../../landsat/LC09_L2SP_038033_20230715_20230717_02_T1/LC09_L2SP_038033_20230715_20230717_02_T1_SR_B1.TIF")
band2 <- raster("../../landsat/LC09_L2SP_038033_20230715_20230717_02_T1/LC09_L2SP_038033_20230715_20230717_02_T1_SR_B2.TIF")
band3 <- raster("../../landsat/LC09_L2SP_038033_20230715_20230717_02_T1/LC09_L2SP_038033_20230715_20230717_02_T1_SR_B3.TIF")
band4 <- raster("../../landsat/LC09_L2SP_038033_20230715_20230717_02_T1/LC09_L2SP_038033_20230715_20230717_02_T1_SR_B4.TIF")
band5 <- raster("../../landsat/LC09_L2SP_038033_20230715_20230717_02_T1/LC09_L2SP_038033_20230715_20230717_02_T1_SR_B5.TIF")
band6 <- raster("../../landsat/LC09_L2SP_038033_20230715_20230717_02_T1/LC09_L2SP_038033_20230715_20230717_02_T1_SR_B6.TIF")
band7 <- raster("../../landsat/LC09_L2SP_038033_20230715_20230717_02_T1/LC09_L2SP_038033_20230715_20230717_02_T1_SR_B7.TIF")


# Read Landsat imagery stack
landsat_stack <- stack(band1,band2,band3,band4,band5,band6,band7)

#setting extent
e <- as(extent(360000, 390000, 4235000, 4265000),'SpatialPolygons')

#cropping
cropped <- crop(landsat_stack,e)

# 4/2 (iron-oxides), 6/7 (hydroxyl and clay min- erals) and 6/5 (ferrous minerals) 
iron_oxides <- (cropped[[4]])/(cropped[[2]])
cropped[[8]] = iron_oxides

p1 <- levelplot(cropped[[8]], 
          col.regions = viridis(200),
          main = "Iron Oxides")

#creating hydroxyl band
hydroxyl <- (cropped[[6]])/(cropped[[7]])
cropped[[9]] = hydroxyl

p2 <- levelplot(cropped[[9]], 
          col.regions = viridis(200),
          main = "Hydroxyl")

#creating ferrous band
ferrous <- (cropped[[6]])/(cropped[[5]])
cropped[[10]] = ferrous

p3 <- levelplot(cropped[[10]], 
          col.regions = viridis(200),
          main = "Ferrous")




# Arrange plots side by side
p4 <- grid.arrange(p1, p2, p3, ncol = 3)

# Save the levelplot as a PNG file
png("./iron_oxides.png", width = 7.69 * 100, height = 8.35 * 100)
print(p1)
dev.off()

# Save the levelplot as a PNG file
png("./hydroxyl.png", width = 7.69 * 100, height = 8.35 * 100)
print(p2)
dev.off()

# Plot the original RGB raster
par(mfrow = c(3, 3))
plotRGB(cropped, r=4,g=3,b=2, main = "Original RGB",stretch="lin")



# Load two example images
img1 <- load.image("./iron_oxides.png")
img2 <- load.image("./hydroxyl.png")











# performing the sabins ratio  It is used here to show the relative spatial distribution of the different 
# alteration zones,with iron-oxide, clay-hydroxyl and ferrous minerals dominated areas respectively in pink, green and blue. 
plotRGB(cropped, r = 8, g = 9, b = 10, axes = FALSE, 
        stretch = "lin", main = "Sabins Ratio")

# Customize x and y axes
axis(side = 1, labels = TRUE, cex.axis = 0.8)  # X-axis
axis(side = 2, labels = TRUE, cex.axis = 0.8)  # Y-axis

# Add labels to x and y axes
mtext("Easting", side = 1, line = 3, cex = 1.2)
mtext("Northing", side = 2, line = 3, cex = 1.2)

#adding a legend manually
legend("bottomright", legend = c("Iron-oxide", "clay-hydroxyl", "ferrous"), fill = c("red","green","blue"))



cropped <- rast(cropped)
#snow index
# Assuming your bands are in the correct order (NIR is 3rd band, SWIR is 6th band)
ndsi <- (cropped[[3]]-cropped[[6]])/(cropped[[6]]+cropped[[3]])

plot(ndsi)
#NDSI threshold of 0.4.
threshold_value <- -0.05  # Adjust this threshold as needed

# Create a binary mask based on the threshold
not_snow_mask <- ndsi >= threshold_value

plot(not_snow_mask)
# Subset the original raster using the complement of the mask
non_snow_raster <- cropped * not_snow_mask

plot(non_snow_raster)


#reading in beaver quad
beaver <- terra::rast("../../Quads/beaver_quad/Beaver.tif")
beaver <- terra::rectify(beaver)
cropped_beaver <- crop(beaver,e)

plotRGB(cropped_beaver, r = 1, g = 2, b = 3, axes = TRUE, 
        stretch = "lin",add=FALSE)


#ranger package random forest
#classify yes/no 


# Perform PCA
pca_model <- prcomp(t(values(cropped)), scale. = TRUE)

# Extract the principal components
pca_components <- pca_model$rotation

# Visualize the first few principal components
plot(pca_components[, 1], type = "l", xlab = "Bands", ylab = "Principal Component 1")
