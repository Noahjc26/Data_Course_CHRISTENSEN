library(rasterVis)
library(raster)
library(tidyverse)
library(viridis)
library(terra)
library(randomForest)
library(e1071)

#grabbing all aster files
directory_path <- "../../ASTER/2005_10_01/AST_L1B_00310012005182513_20231130134446_13225/"

# Use list.files with pattern argument to filter files
tif_files <- list.files(directory_path, pattern = "\\.tif$", full.names = TRUE)

#setting extent
e <- as(extent(360000, 390000, 4235000, 4265000),'SpatialPolygons')

#turning into raster, rectifying, cropping, and aggregating
band1 <- rast(tif_files[12]) %>% rectify() %>% crop(e) %>% aggregate(2)
band2 <- rast(tif_files[13]) %>% rectify() %>% crop(e) %>% aggregate(2)
band3B <- rast(tif_files[14]) %>% rectify() %>% crop(e) %>% aggregate(2)
band3N <- rast(tif_files[15]) %>% rectify() %>% crop(e) %>% aggregate(2)
band4 <- rast(tif_files[1]) %>% rectify() %>% crop(e) %>% resample(band1)
band5 <- rast(tif_files[2]) %>% rectify() %>% crop(e) %>% resample(band1)
band6 <- rast(tif_files[3]) %>% rectify() %>% crop(e) %>% resample(band1)
band7 <- rast(tif_files[4]) %>% rectify() %>% crop(e) %>% resample(band1)
band8 <- rast(tif_files[5]) %>% rectify() %>% crop(e) %>% resample(band1)
band9 <- rast(tif_files[6]) %>% rectify() %>% crop(e) %>% resample(band1)


#ASTER Gains 01 HGH, 02 HGH, 3N NOR, 04 NOR, 05 NOR, 06 NOR, 07 NOR, 08 NOR, 09 NOR
#DN to spectral radiance using normal gain
band1 = (band1-1)*0.676
band2 = (band2-1)*0.708
band3N = (band3N-1)*0.862
band3B = (band3B-1)*0.862
band4 = (band4-1)*0.2174
band5 = (band5-1)*0.0696
band6 = (band6-1)*0.0625
band7 = (band7-1)*0.0597
band8 = (band8-1)*0.0417
band9 = (band9-1)*0.0318


#TOA reflectance
Solar_Elevation_Angle <- 46.302721
d <- 1-0.01672*cos(0.9856*(274-4)*(pi/180))
z <- 90-Solar_Elevation_Angle
B1_ESUN <- 1845.99
B2_ESUN <- 1555.74
B3_ESUN <- 1119.47
B4_ESUN <- 231.25
B5_ESUN <- 79.81
B6_ESUN <- 74.99
B7_ESUN <- 68.88
B8_ESUN <- 59.74
B9_ESUN <- 56.92

band1 = ((pi*band1*d^2)/(B1_ESUN*cos(z)))
band2 = ((pi*band2*d^2)/(B2_ESUN*cos(z)))
band3N = ((pi*band3N*d^2)/(B3_ESUN*cos(z)))
band3B = ((pi*band3B*d^2)/(B3_ESUN*cos(z)))
band4 = ((pi*band4*d^2)/(B4_ESUN*cos(z)))
band5 = ((pi*band5*d^2)/(B5_ESUN*cos(z)))
band6 = ((pi*band6*d^2)/(B6_ESUN*cos(z)))
band7 = ((pi*band7*d^2)/(B7_ESUN*cos(z)))
band8 = ((pi*band8*d^2)/(B8_ESUN*cos(z)))
band9 = ((pi*band9*d^2)/(B9_ESUN*cos(z)))

#stacking all bands
aster <- c(band1,band2,band3B,band3N,band4,band5,band6,band7,band8,band9)

#reading in if not already ran
cropped <- stack("../../ASTER/2005_10_01/full_cleaned.tif")

#reading in saved training points
training_points <- readRDS("./polygon_training_data_aster.rds")

#turning into data frame
df <- data.frame(training_points)

#creating model based on column "class" 
model.class <- rpart(as.factor(Class)~.,
                     data = df,
                     method = 'class',
                     control = rpart.control("minsplit" = 1))

#plotting the model as a tree
rpart.plot(model.class, box.palette = 4, main = "Classification Tree")

#not sure exactly why but the prediction plot likes it in rast format
cropped <- rast(cropped)

# setting band names
new_band_names <- paste0("Band", 1:10)

# Rename the layers
names(cropped) <- new_band_names

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
plotRGB(cropped_masked, r=1,g=2,b=3, main = "Original RGB",stretch="lin")

#band 2 red, band 3 nir for NDVI
#NDVI = (aster[[3]] - aster[[2]])/(aster[[3]] + aster[[2]])
#
#creating true/false threshold
#ndvi_pixels = NDVI > 0.4
#
#plot(ndvi_pixels)
#
#
#creating NDSI (Band1 - Band3)/ (Band1 + Band3) (green-NIR)/(green+NIR)
#NDSI = (aster[[1]]-aster[[3]])/(aster[[1]]+aster[[3]])
#
#creating true/false threshold
#ndsi_pixels = NDSI > -0.30
#
#plot(ndsi_pixels)
#
#combined_mask <- ndsi_pixels | ndvi_pixels
#
#plot(combined_mask)
#
# Apply the mask to the original raster
#aster_masked <- aster
#
#aster_masked[combined_mask] <- NA
#
#aster <- aster_masked




aster <- cropped_masked






names(aster)
#band 5/7
band11 <- (aster[[6]]/aster[[8]])

#band 5/4
band12 <- (aster[[6]]/aster[[5]])

#band 4/5
band13 <- (aster[[5]]/aster[[6]])

#b6/b8 * b9/b8 calcite index
band14 <- (aster[[7]]/aster[[9]])*(aster[[10]]/aster[[9]])

#(b7+b9)/b8 kaolinite, sericite, chlorite and epidoteminerals,
band15 <- (aster[[8]]+aster[[10]])/aster[[9]]

#band7/band6
band16 <- (aster[[8]]/aster[[7]])

#band7/Band5
band17 <- (aster[[8]]/aster[[6]])

#band5/Band6
band18 <- (aster[[6]]/aster[[7]])

#adding all new ratios
aster2 <- stack(c(aster,band11,band12,band13,band14,band15,band16,band17,band18))

#dvanced  argillic alterations  (Al–OH minerals) https://www.researchgate.net/publication/290434274_ASTER_spectral_band_ratios_for_detection_of_hydrothermal_alterations_and_ore_deposits_in_the_Panagyurishte_Ore_Region_Central_Srednogorie_Bulgaria
plotRGB(aster2,18,16,17,stretch = "lin")

aster2 <- rast(aster2)
smoothed_raster <- focal(aster2, w = matrix(1, ncol = 3, nrow = 3), fun = mean, na.rm = TRUE)


plotRGB(smoothed_raster, r = 11, g = 12, b = 4, axes = FALSE, 
        stretch = "hist")


#looking at alteration zones
plotRGB(aster2,11,12,4,stretch="lin",main = "Alteration zones",axes=FALSE)

# off whitish cream color represents hydrothermal alteration
color_palette <- rev(rainbow(100, end = 0.7))

# Plot the raster using levelplot with the custom color palette
levelplot(aster2[[13]], main = "Hydrothermal Alteration", stretch = "lin", col.regions = color_palette)


#calcite index
levelplot(aster2[[14]],
          main = "Calcite index", stretch = "lin")


#kaolinite, sericite, chlorite and epidoteminerals,

levelplot(aster2[[15]],
          main = "kaolinite, sericite, chlorite and epidote minerals", stretch = "lin")

