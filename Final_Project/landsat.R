#PCA
library(rasterVis)
library(raster)
library(tidyverse)
library(viridis)
library(gridExtra)
library(shiny)
library(shinyjs)
library(imager)
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


band1 <- raster("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1_SR_B1.TIF")
band2 <- raster("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1_SR_B2.TIF")
band3 <- raster("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1_SR_B3.TIF")
band4 <- raster("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1_SR_B4.TIF")
band5 <- raster("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1_SR_B5.TIF")
band6 <- raster("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1_SR_B6.TIF")
band7 <- raster("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1/LC09_L2SP_038033_20231019_20231020_02_T1_SR_B7.TIF")


# Read Landsat imagery stack
landsat_stack <- stack(band1,band2,band3,band4,band5,band6,band7)

#setting extent
e <- as(extent(360000, 390000, 4235000, 4265000),'SpatialPolygons')

#cropping
cropped <- crop(landsat_stack,e)

writeRaster(cropped,"../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/cropped.tif")


cropped <- stack("../../landsat/LC09_L2SP_038033_20230715_20230717_02_T1/cropped.tif")







# 4/2 (iron-oxides), 6/7 (hydroxyl and clay min- erals) and 6/5 (ferrous minerals) 
iron_oxides <- (cropped[[4]])/(cropped[[2]])
cropped[[8]] = iron_oxides

levelplot(cropped[[8]], 
          col.regions = viridis(200),
          main = "Iron Oxides")

#creating hydroxyl band
hydroxyl <- (cropped[[6]])/(cropped[[7]])
cropped[[9]] = hydroxyl

levelplot(cropped[[9]], 
          col.regions = viridis(200),
          main = "Hydroxyl")

#creating ferrous band
ferrous <- (cropped[[6]])/(cropped[[5]])
cropped[[10]] = ferrous

levelplot(cropped[[10]], 
          col.regions = viridis(200),
          main = "Ferrous")









# performing the sabins ratio  It is used here to show the relative spatial distribution of the different 
# alteration zones,with iron-oxide, clay-hydroxyl and ferrous minerals dominated areas respectively in pink, green and blue. 
plotRGB(cropped, r = 8, g = 9, b = 10, axes = FALSE, 
        stretch = "lin", main = "Sabins Ratio")



#adding a legend manually
legend("bottomright", legend = c("Iron-oxide", "clay-hydroxyl", "ferrous"), fill = c("red","green","blue"))



cropped <- rast(cropped)


#reading in beaver quad
beaver <- terra::rast("../../Quads/beaver_quad/Beaver.tif")
beaver <- terra::rectify(beaver)
cropped_beaver <- crop(beaver,e)

plotRGB(cropped_beaver, r = 1, g = 2, b = 3, axes = TRUE, 
        stretch = "lin",add=FALSE)

writeRaster(cropped_beaver,"../../Quads/beaver_quad/cropped_beaver.tif")
#ranger package random forest
#classify yes/no 


#----------------------------------------
plotRGB(cropped, r=4,g=3,b=2, main = "Original RGB",stretch="lin")

#applying supervised classification 1
snow <- draw(x="points",n=100)

cropped <- rast(cropped)

#turning into dataframe and extracting values
snow_df <- terra::extract(cropped,snow)

#renaming to band names
# colnames(snow_df) <- sapply(strsplit(colnames(snow_df), "_"), function(x) x[2])

#finding mean of all columns
snow_df <- as.data.frame.list(colMeans(snow_df))

#adding column "class" with values "snow"
snow_df$class = "snow"

#pivoting longer
snow_df_long <- snow_df %>% 
  pivot_longer(cols = starts_with("L"),
               values_to = "reflectance",
               names_to = "bands")

saveRDS(snow_df_long,"./snow_classification_landsat_LC09_L2SP_038033_20231019_20231020_02_T1.rds")


#applying supervised classification 2
baren <- draw(x="points",n=40)
#turning into dataframe and extracting values
baren_df <- terra::extract(cropped,baren)

#renaming to band names
# colnames(green_df) <- sapply(strsplit(colnames(green_df), "_"), function(x) x[2])

#finding mean of all columns
baren_df <- as.data.frame.list(colMeans(baren_df))

#adding column "class" with values "green"
baren_df$class = "baren"

#pivoting longer
baren_df_long <- baren_df %>% 
  pivot_longer(cols = starts_with("L"),
               values_to = "reflectance",
               names_to = "bands")

saveRDS(baren_df_long,"./baren_classification_landsat_LC09_L2SP_038033_20231019_20231020_02_T1.rds")

df_long <- readRDS("./baren_classification_landsat_LC09_L2SP_038033_20231019_20231020_02_T1.rds")

df_long$class = barren
#applying supervised classification 3
vegetation <- draw(x="points",n=40)
#turning into dataframe and extracting values
vegetation_df <- terra::extract(cropped,vegetation)

#renaming to band names
# colnames(rock_df) <- sapply(strsplit(colnames(rock_df), "_"), function(x) x[2])

#finding mean of all columns
vegetation_df <- as.data.frame.list(colMeans(vegetation_df))

#adding column "class" with values "rock"
vegetation_df$class = "vegetation"

#pivoting longer
vegetation_df_long <- vegetation_df %>% 
  pivot_longer(cols = starts_with("L"),
               values_to = "reflectance",
               names_to = "bands")

saveRDS(vegetation_df_long,"./vegetation_classification_landsat_LC09_L2SP_038033_20231019_20231020_02_T1.rds")

#joining all of the dataframes together
part_df <- full_join(vegetation_df_long,snow_df_long)
full_df <- full_join(part_df,baren_df_long)

full_df <- full_df %>% 
  select(-ID)
#plotting the dataframes
full_df %>% 
  ggplot(aes(x=bands,y=reflectance,color=class)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,size = 4))

#combining all of the dataframes
training_points = rbind(vegetation_df,snow_df,baren_df)

saveRDS(training_points,"./training_points_landsat_LC09_L2SP_038033_20231019_20231020_02_T1.rds")

training_points <- readRDS("./training_points_landsat_LC09_L2SP_038033_20231019_20231020_02_T1.rds")



#turning into data frame
df <- data.frame(training_points)

df <- df %>% 
  select(-ID)
#creating model based on column "class" 
model.class <- rpart(as.factor(class)~.,
                     data = df,
                     method = 'class',
                     control = rpart.control("minsplit" = 1))

#plotting the model as a tree
rpart.plot(model.class, box.palette = 4, main = "Classification Tree")


#making prediction plot
pr <- predict(cropped, model.class, type ='class', progress = 'text') %>% 
  raster()

#looking at values
values(pr)


#plotting
levelplot(pr, maxpixels = 1e6,
          main = "Supervised Classification of Imagery")

#saving classification
writeRaster(pr,"../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/classification.tif")

#making a copy to create a mask
pr_copy <- pr

# Set all values not equal to 1 to NoData or another value
pr_copy[pr_copy != 1] <- NA  # You can replace NA with any other value

# Plot the modified raster
plot(pr_copy)

#turning into raster
pr_copy <- rast(pr_copy)

writeRaster(pr_copy,"../../landsat/classification_mask.tif")

# Use the mask function
cropped_masked <- terra::mask(cropped, pr_copy)

# Now 'cropped_masked' contains the values from 'cropped' where 'pr_copy' has values equal to 1, and other values are set to NA

# Plot the masked raster
plotRGB(cropped_masked, r=4,g=3,b=2, main = "Original RGB",stretch="lin")

writeRaster(cropped_masked,"../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/cropped_masked.tif")





#-------------

#now to perform. everything except this time we have a masked raster
cropped <- stack("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/cropped_masked.tif")

# 4/2 (iron-oxides), 6/7 (hydroxyl and clay min- erals) and 6/5 (ferrous minerals) 
iron_oxides <- (cropped[[4]])/(cropped[[2]])
cropped[[8]] = iron_oxides

breaks <- c(1.35,2.5)

levelplot(cropped[[8]], 
          main = "Iron Oxides",
          colorkey=FALSE,
          col.regions =  "green",
          margin = FALSE,
          at = breaks)

#creating hydroxyl band
hydroxyl <- (cropped[[6]])/(cropped[[7]])
cropped[[9]] = hydroxyl

# Set up breaks for the color scale
breaks <- c(1.28,2.5)

levelplot(cropped[[9]], 
          main = "Hydroxyl",
          colorkey=FALSE,
          col.regions =  "green",
          margin = FALSE,
          at = breaks)

#creating ferrous band
ferrous <- (cropped[[6]])/(cropped[[5]])
cropped[[10]] = ferrous

# Set up breaks for the color scale
breaks <- c(1.25,2.5)

levelplot(cropped[[10]],
          at = breaks,
          margin = FALSE,
          col.regions = "green",
          colorkey = FALSE,
          main = "Ferrous")









# performing the sabins ratio  It is used here to show the relative spatial distribution of the different 
# alteration zones,with iron-oxide, clay-hydroxyl and ferrous minerals dominated areas respectively in pink, green and blue. 
plotRGB(cropped, r = 8, g = 9, b = 10, axes = FALSE, 
        stretch = "lin", main = "Sabins Ratio")





