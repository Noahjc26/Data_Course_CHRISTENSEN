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
library(leaflet)

#reading in hyperion image
surf_reflectance <- rast("../../Hyperion/corrected_EO1H0380332004336110PZ_1T.tif")

#looking at full hyperion image
surf_reflectance %>% 
plotRGB(31,20,10,stretch = "lin")

#setting cropping extent
e <- as(extent(330000, 342000, 4232000, 4245000), 'SpatialPolygons')

#cropping image
cropped <- crop(surf_reflectance,e)

#viewing new cropped image
cropped %>% 
  plotRGB(60,20,30,stretch = "lin")

#loading in beaver quad
beaver <- rast("../../Quads/Beaver_NW_Quad/ofr-729/BeaverNW__GeoTiff_NAD27.tif")

#making beaver_quad have the same crs as surf_reflectance
test <- project(beaver,crs(cropped))

#cropping new projected geologic map of beaver
beav <- crop(test,e)

#comparing plots of geologic map and Hyperion satellite imagery
plotRGB(beav,1,2,3)
plotRGB(cropped,31,20,10,stretch = "lin")

#reading in metadata for hyperion image
md <- read_lines("../../Hyperion/mineral_mountain/EO1H0380332004336110PZ_1T/EO1H0380332004336110PZ_MTL_L1T.TXT")



#----
#Lets see what the classification would look like if given all the reflectance values and all the minerals in the spectral library


#reading in minerals
min <- readRDS("./hyperion_mineral_signatures_cleaned.rds")
unique(min$mineral)
levels(min$mineral)
#getting the average for all minerals since there are multiple values for each band for certain minerals
full_averaged <- min %>%
  group_by(mineral) %>%
  summarise(across(starts_with("B"), mean))

#making sure mineral is a factor
full_averaged$mineral <- as.factor(full_averaged$mineral)

# Train the Random Forest model
rf_model <- randomForest(mineral ~ ., data = full_averaged, ntree = 501,na.action = na.omit)  # Adjust ntree and other parameters as needed

#looking at the variable of importance plot
varImpPlot(rf_model)

#making the classified raster
classified_raster <- predict(cropped, rf_model,na.omit = TRUE)

#plotting the classified raster
plot(classified_raster)


#----
# now lets try but with a subset of minerals since that classification sucked
beaver_minerals <- filter(min, mineral == c("quartz","dolomite"))

# Remove unused levels from factor variables in the subset
beaver_minerals <- droplevels(beaver_minerals)

#getting the average for all minerals since there are multiple values for each band for certain minerals
beaver_averaged <- beaver_minerals %>%
  group_by(mineral) %>%
  summarise(across(starts_with("B"), mean))


#- lets check out what these minerals plot as spectrally
long <- beaver_averaged %>% 
  pivot_longer(starts_with("B"),
               names_to = "Bands")

#reading in hyperion band info
band_info <- read_rds("./hyperion_band_info.rds")

full <- full_join(band_info,long)

full %>% 
  ggplot(aes(x=Wavelength_nm,y=value,color=mineral)) +
  geom_point(size=.5)

#-

#making sure mineral is a factor
beaver_averaged$mineral <- as.factor(beaver_averaged$mineral)

# Train the Random Forest model
rf_model <- randomForest(mineral ~ ., data = beaver_averaged, ntree = 1000,na.action = na.omit)  # Adjust ntree and other parameters as needed

#looking at the variable of importance plot
varImpPlot(rf_model)

#making the classified raster
classified_raster <- predict(cropped, rf_model,na.omit = TRUE)

#plotting the classified raster
plot(classified_raster)
plotRGB(beav,1,2,3)


#----
#now lets try it with reducing the bands it uses since that classification still sucks

# Define the absorption feature range
absorption_range <- c(1600, 2200)  # Adjust this based on your spectral data
#this will leave bands 146-204

#reducing the wavlengths to only between 1600 and 2200
part <- full %>%
  filter(Wavelength_nm >= 1600, Wavelength_nm <= 2200)

#plotting reduced wavelength for the 4 minerals
part %>% 
  ggplot(aes(x=Wavelength_nm,y=value,color=mineral)) +
  geom_point(size=.5)

#pivoting wider and removing unnecessary colummns
wide_part <- part %>% 
  select(-c(Satellite_Name,Wavelength_Units,Wavelength_nm,FWHM_nm,Description,Irradiance,Irradiance_Units,Rad_Conv)) %>% 
  pivot_wider(names_from = Bands, values_from = value)

# Train the Random Forest model
rf_model <- randomForest(mineral ~ ., data = wide_part, ntree = 501,na.action = na.omit)  # Adjust ntree and other parameters as needed

#looking at the variable of importance plot
varImpPlot(rf_model)

#making the classified raster
classified_raster <- predict(cropped, rf_model,na.omit = TRUE)

#plotting the classified raster
plot(classified_raster)
plotRGB(beav,1,2,3)

#----







ggplot() +
  geom_spatraster_rgb(data=test)
