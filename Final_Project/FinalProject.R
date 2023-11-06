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


surf_reflectance <- readRDS("./corrected_EO1H0380332014325110PZ_1T.rds")

e <- as(extent(291500, 300000, 4236500, 4245000), 'SpatialPolygons')

#plotting RGB by bands
surf_reflectance %>% 
plotRGB(r=31,g=20,b=10,stretch = "lin")

Milford_quad <- rast("../../Quads/Milford_Frisco_Quad/ofr-674/OFR-674DM_Milford-EastHalfFriscoQuads_DataView.tif")


cropped_surf <- terra::crop(surf_reflectance,e)
cropped_milford <- crop(Milford_quad,e)

#making Milford_quad have the same crs as surf_reflectance
test <- project(cropped_milford,crs(surf_reflectance))


#_____

#now to maybe make a classification

min_sig <- readRDS("./cleaned_mineral_signatures.rds")

# Remove everything before and including the underscore
min_sig$mineral <- sub(".*_", "", min_sig$mineral)

#setting as a factor
min_sig$mineral <- as.factor(min_sig$mineral)

#thats a lot of minerals
unique(min_sig$mineral)

#reducing to a few minerals
reduced_min_sig <- subset(min_sig, mineral == c("biotite","quartz","microcline","dolomite"))

reduced_min_sig$mineral <- as.factor(reduced_min_sig$mineral)



full_averaged_df <- min_sig %>%
  group_by(mineral) %>%
  summarise(across(starts_with("B"), mean))

full_averaged_df$mineral <- as.factor(full_averaged_df$mineral)

# Train the Random Forest model
rf_model <- randomForest(mineral ~ ., data = full_averaged_df, ntree = 1500,na.action = na.omit)  # Adjust ntree and other parameters as needed


classified_raster <- predict(cropped_surf, rf_model,na.omit = TRUE)


plot(classified_raster)

ggplot() +
  geom_spatraster_rgb(data=test)


long_r_m_s <- reduced_min_sig %>% 
  pivot_longer(cols = starts_with("B"),
               names_to = "Band",
               values_to = "Reflectance")

long_r_m_s %>% 
  ggplot(aes(x=Band,y=Reflectance,color=mineral)) +
  geom_point(size=0.25)

#creating a dataframe. that is the average reflectance values for each mineral
averaged_df <- reduced_min_sig %>%
  group_by(mineral) %>%
  summarise(across(starts_with("B"), mean))

long_averaged_df <- averaged_df %>% 
  pivot_longer(cols = starts_with("B"),
               names_to = "Band",
               values_to = "Reflectance")

long_averaged_df %>% 
  ggplot(aes(x=Band,y=Reflectance,color=mineral)) +
  geom_point(size=0.25)





#testing the draw and extract functions
# features <- draw(x="points",n=4)
# dftest <- terra::extract(Surf_Reflectance,features) %>% 
#   t()
# plot(dftest[,1])
# head(dftest)


#applying supervised classification 1
snow <- draw(x="points",n=4)

#turning into dataframe and extracting values
snow_df <- terra::extract(surf_reflectance,snow)

#renaming to band names
# colnames(snow_df) <- sapply(strsplit(colnames(snow_df), "_"), function(x) x[2])

#finding mean of all columns
snow_df <- as.data.frame.list(colMeans(snow_df))

#adding column "class" with values "snow"
snow_df$class = "snow"

#pivoting longer
snow_df_long <- snow_df %>% 
pivot_longer(cols = starts_with("E"),
             values_to = "reflectance",
             names_to = "bands")



#applying supervised classification 2
green <- draw(x="points",n=4)
#turning into dataframe and extracting values
green_df <- terra::extract(surf_reflectance,green)

#renaming to band names
# colnames(green_df) <- sapply(strsplit(colnames(green_df), "_"), function(x) x[2])

#finding mean of all columns
green_df <- as.data.frame.list(colMeans(green_df))

#adding column "class" with values "green"
green_df$class = "green"

#pivoting longer
green_df_long <- green_df %>% 
  pivot_longer(cols = starts_with("E"),
               values_to = "reflectance",
               names_to = "bands")



#applying supervised classification 3
rock <- draw(x="points",n=4)
#turning into dataframe and extracting values
rock_df <- terra::extract(surf_reflectance,rock)

#renaming to band names
# colnames(rock_df) <- sapply(strsplit(colnames(rock_df), "_"), function(x) x[2])

#finding mean of all columns
rock_df <- as.data.frame.list(colMeans(rock_df))

#adding column "class" with values "rock"
rock_df$class = "rock"

#pivoting longer
rock_df_long <- rock_df %>% 
  pivot_longer(cols = starts_with("E"),
               values_to = "reflectance",
               names_to = "bands")



#joining all of the dataframes together
part_df <- full_join(green_df_long,snow_df_long)
full_df <- full_join(part_df,rock_df_long)

full_df <- full_df %>% 
  select(-ID)
#plotting the dataframes
full_df %>% 
  ggplot(aes(x=bands,y=reflectance,color=class)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,size = 4))

#combining all of the dataframes
training_points = rbind(green_df,snow_df,rock_df)

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
rpart.plot(model.class, box.palette = 3, main = "Classification Tree")



pr <- predict(surf_reflectance, model.class, type ='class', progress = 'text') %>% 
  raster()

values(pr)
levelplot(pr, maxpixels = 1e6,
          scales=list(draw=FALSE),
          main = "Supervised Classification of Imagery")


surf_reflectance %>% 
  plotRGB(r=31,g=20,b=10,stretch = "hist")


names(surf_reflectance)


#reading in mineral signature data frame
min_sig <- read_rds("./cleaned_mineral_signatures.rds")


long_min %>% 
  ggplot(aes(x=bands,y=reflectance)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,size = 4))

#creating model based on column "class" 
model.class <- rpart(as.factor(mineral)~.,
                     data = min_sig,
                     method = 'class')
model.class
#plotting the model as a tree
rpart.plot(model.class, box.palette = 3, main = "Classification Tree")




