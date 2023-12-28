
cropped <- rast(cropped)

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