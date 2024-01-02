library(rasterVis)
library(raster)
library(tidyverse)
library(viridis)
library(gridExtra)
library(shiny)
library(raster)
library(tidyverse)
library(sf)
library(rpart)
library(rpart.plot)
library(rasterVis)
library(mapedit)
library(mapview)
library(caret)
library(forcats)
library(grid)
library(terra)

#stacking all bands
bands <- stack(lapply(1:7, function(i) raster(paste0("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1_SR_B", i, ".TIF"))))

#setting extent
e <- as(extent(360000, 390000, 4235000, 4265000),'SpatialPolygons')

#cropping by extent
cropped <- crop(bands,e)

# Plot the masked raster
plotRGB(cropped, r=4,g=3,b=2, main = "Original RGB",stretch="lin")

#reading in saved training points
training_points <- readRDS("./training_points_landsat_LC09_L2SP_038033_20231019_20231020_02_T1.rds")

#turning into data frame
df <- data.frame(training_points)

#removing ID column
df <- df %>% 
  select(-ID)

#creating model based on column "class" 
model.class <- rpart(as.factor(class)~.,
                     data = df,
                     method = 'class',
                     control = rpart.control("minsplit" = 1))

#plotting the model as a tree
rpart.plot(model.class, box.palette = 4, main = "Classification Tree")

#not sure exactly why but the prediction plot likes it in rast format
cropped <- rast(cropped)

#making prediction plot
pr <- predict(cropped, model.class, type ='class', progress = 'text') %>% 
  raster()

#looking at values
unique(values(pr))

#plotting
levelplot(pr, maxpixels = 1e6,
          main = "Supervised Classification of Imagery")

#saving classification
#writeRaster(pr,"../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/classification.tif")

#making a copy to create a mask
pr_copy <- pr

# Set all values not equal to 1 to NoData or another value
pr_copy[pr_copy != 1] <- NA  # You can replace NA with any other value

#turning into rast format
pr_copy <- rast(pr_copy)

#writeRaster(pr_copy,"../../landsat/classification_mask.tif")

# Use the mask function
cropped_masked <- terra::mask(cropped, pr_copy)

# Now 'cropped_masked' contains the values from 'cropped' where 'pr_copy' has values equal to 1, and other values are set to NA

# Plot the masked raster
plotRGB(cropped_masked, r=4,g=3,b=2, main = "Original RGB",stretch="lin")



# NDVI to get rid of remaining vegetation pixels (Band 5 - Band 4) / (Band 5 + Band 4)
NDVI = (cropped_masked[[5]]-cropped_masked[[4]])/(cropped_masked[[5]]+cropped_masked[[4]])

NDVI_copy = NDVI

NDVI_copy[NDVI_copy > 0.10] <- NA

plot(NDVI_copy)

# NDSI  (Band 3 – Band 6) / (Band 3 + Band 6)
NDSI =  (cropped_masked[[3]]-cropped_masked[[6]])/(cropped_masked[[3]]+cropped_masked[[6]])

NDSI_copy = NDSI

NDSI_copy[NDSI_copy > 0] <- NA

plot(NDSI_copy)


# Apply the combined mask to the original raster
masked_cropped <- mask(cropped_masked, NDSI_copy)

# Apply the combined mask to the original raster
masked_cropped <- mask(masked_cropped, NDVI_copy)

plot(masked_cropped)

cropped_masked = masked_cropped

cropped_masked = stack(cropped_masked)
#writeRaster(cropped_masked,"../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/cropped_masked.tif")

# 4/2 (iron-oxides), 6/7 (hydroxyl and clay minerals) and 6/5 (ferrous minerals) 
cropped_masked[[8]] <- (cropped_masked[[4]])/(cropped_masked[[2]])

#setting breaks for iron oxides
breaks <- c(1.35,2.5)

levelplot(cropped_masked[[8]], 
          main = "Iron Oxides",
          colorkey=FALSE,
          col.regions =  "green",
          margin = FALSE,
          at = breaks)

#creating hydroxyl band
cropped_masked[[9]] <- (cropped_masked[[6]])/(cropped_masked[[7]])

# Set up breaks for the hydroxyl
breaks <- c(1.28,2.5)

levelplot(cropped_masked[[9]], 
          main = "Hydroxyl",
          colorkey=FALSE,
          col.regions =  "green",
          margin = FALSE,
          at = breaks)

#creating ferrous band
cropped_masked[[10]] <- (cropped_masked[[6]])/(cropped_masked[[5]])

# Set up breaks for the Ferrous
breaks <- c(1.25,2.5)

levelplot(cropped_masked[[10]],
          at = breaks,
          margin = FALSE,
          col.regions = "green",
          colorkey = FALSE,
          main = "Ferrous")


# performing the sabins ratio  It is used here to show the relative spatial distribution of the different 
# alteration zones,with iron-oxide, clay-hydroxyl and ferrous minerals dominated areas respectively in pink, green and blue. 

plotRGB(cropped_masked, r = 8, g = 9, b = 10, axes = FALSE, 
        stretch = "lin", main = "Sabins Ratio")

cropped_masked <- rast(cropped_masked)

smoothed_raster <- focal(cropped_masked, w = matrix(1, ncol = 5, nrow = 5), fun = mean, na.rm = TRUE)

smoothed_raster = stack(smoothed_raster)

plotRGB(smoothed_raster, r = 8, g = 9, b = 10, axes = FALSE, 
        stretch = "lin", main = "Sabins Ratio")

