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
library(sf)

hyperion_milford <- rast("../../Hyperion/cropped_corrected_EO1H0380332014325110PZ_1T.tif")
geo_map_milford <- rast("../../Quads/Milford_Frisco_Quad/cropped.tif")
hyperion_band_info <- read_rds("./cleaned_hyperion_band_info.rds")

hyperion_milford_df <- hyperion_milford %>% 
  as.data.frame(xy=TRUE,cells=TRUE,na.rm=TRUE) #creating dataframe

hyperion_milford_df <- hyperion_milford_df %>%   
  pivot_longer(cols = starts_with("B"), #moving all bands into one column
               names_to = "Bands",
               values_to = "Reflectance") %>%
  rename(easting = x,northing = y)

hyperion_milford_df <- full_join(hyperion_milford_df,hyperion_band_info) %>% 
  clean_names() #cleaning names


e <- as(extent(293500, 298500, 4240000, 4244000), 'SpatialPolygons')
hyperion_milford <- terra::crop(hyperion_milford,e)
geo_map_milford <- terra::crop(geo_map_milford,e)



#plotting RGB by bands
plot <- hyperion_milford %>% 
  plotRGB(r=31,g=20,b=10,stretch = "lin")

geo_map_milford %>% 
  plotRGB(1,2,3)

#applying supervised classification 1
mrs <- draw(x="points",n=25)

#turning into dataframe and extracting values
mrs_df <- terra::extract(hyperion_milford,mrs)

#finding mean of all columns
mrs_df <- as.data.frame.list(colMeans(mrs_df))

#adding column "class" with values "mrs"
mrs_df$class = "mrs"

#pivoting longer
mrs_df_long <- mrs_df %>% 
  pivot_longer(cols = starts_with("B"),
               values_to = "reflectance",
               names_to = "Bands")



#applying supervised classification 2
tbr <- draw(x="points",n=25)
#turning into dataframe and extracting values
tbr_df <- terra::extract(hyperion_milford,tbr)

#finding mean of all columns
tbr_df <- as.data.frame.list(colMeans(tbr_df))

#adding column "class" with values "green"
tbr_df$class = "tbr"

#pivoting longer
tbr_df_long <- tbr_df %>% 
  pivot_longer(cols = starts_with("B"),
               values_to = "reflectance",
               names_to = "Bands")



#applying supervised classification 3
two <- draw(x="points",n=25)
#turning into dataframe and extracting values
two_df <- terra::extract(hyperion_milford,two)

#finding mean of all columns
two_df <- as.data.frame.list(colMeans(two_df))

#adding column "class" with values "rock"
two_df$class = "two"

#pivoting longer
two_df_long <- two_df %>% 
  pivot_longer(cols = starts_with("B"),
               values_to = "reflectance",
               names_to = "Bands")

#applying supervised classification 3
tsp <- draw(x="points",n=25)

#turning into dataframe and extracting values
tsp_df <- terra::extract(hyperion_milford,tsp)

#finding mean of all columns
tsp_df <- as.data.frame.list(colMeans(tsp_df))

#adding column "class" with values "rock"
tsp_df$class = "tsp"

#pivoting longer
tsp_df_long <- tsp_df %>% 
  pivot_longer(cols = starts_with("B"),
               values_to = "reflectance",
               names_to = "Bands")







#joining all of the dataframes together
part_df <- full_join(two_df_long,mrs_df_long)
part_df <- full_join(part_df,tsp_df_long)
full_df <- full_join(part_df,tbr_df_long)

full_df <- full_join(full_df,hyperion_band_info)
#plotting the dataframes
full_df %>% 
  ggplot(aes(x=Wavelength_nm,y=reflectance,color=class)) +
  geom_smooth() +
  theme_bw()

#combining all of the dataframes
training_points = rbind(two_df,tbr_df,mrs_df,tsp_df)

#saving as shapefile
tp_shapefile <- write_sf((rbind(tsp,two,mrs,tsp)), "./calgary_trainingPoints.shp", driver = "ESRI shapefile")

#turning into data frame
df <- data.frame(training_points)


df <- df %>% 
  select(-ID)
#creating model based on column "class" 
model.class <- rpart(as.factor(class)~.,
                     data = df,
                     method = 'class',
                     control = rpart.control("minsplit" = -5))

#plotting the model as a tree
rpart.plot(model.class, main = "Classification Tree")

#turning into raster instead of spatraster
raster_hyperion_milford <- stack(hyperion_milford)

#making prediction
pr <- predict(raster_hyperion_milford, model.class, type ='class', progress = 'text') %>% 
  ratify()

#changing prediction labels
levels(pr) <- levels(pr)[[1]] %>%
  mutate(legend = c("two","tbr","mrs","tsp"))

#making plot of prediction
levelplot(pr, maxpixels = 1e6,
          scales=list(draw=FALSE),
          main = "Supervised Classification of Imagery")

#SUPERvised classification methods
# minimum distance
# maxiumum likilihood
# support vector machines
# k-means
# PCA for multispectral images (there is gonna be more resources.)









head(hyperion_milford_df)
#plotting by cell#
p = hyperion_milford_df %>% 
  filter(cell == 6000)
p2 = hyperion_milford_df %>% 
  filter(cell == 12439)

p <- p %>% 
  ggplot(aes(x=wavelength_nm,y=reflectance)) +
  geom_point()

p2 <- p2 %>% 
  ggplot(aes(x=wavelength_nm,y=reflectance)) +
  geom_line()

plot_grid(p, p2, labels = "AUTO")


#plotting by band#
map <- df %>% 
  filter(bands == "B030") 

map <- hyperion_milford_df %>% 
  ggplot() +
  geom_raster(aes(x = easting, y = northing, fill = reflectance, text = cell)) +
  theme_classic()

ggplotly(map, tooltip = c("cell","easting","northing"))
