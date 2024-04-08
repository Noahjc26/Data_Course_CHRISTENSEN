library(tidyverse)
library(terra)
library(RColorBrewer)
library(broom)

df <- read.csv("./task_7/UTSNTL_META.csv")

# 1
df_vect <- vect(df,geom=c("Longitude", "Latitude"))

plot(df_vect,"Elevation..ft.")

# 2
df2 <- read.csv("./task_7/cleaned_snow.csv")


#turning date column into date format
df2$Date <- as.Date(df2$Date)
df_2021_winter <- df2[df2$Date < "2022-03-21" & df2$Date > "2021-12-21",]

#only keeping row with the highest snow depth
df_max_snow_depth <- filter(df_2021_winter, grepl("Snow_Depth", data)) %>% 
  arrange(desc(Value)) %>%
  distinct(location, .keep_all = TRUE)

df_max_snow_depth <- df_max_snow_depth %>% 
  rename(Station.Name = location)


df$Station.Name <- gsub(" ","_", df$Station.Name)
df$Station.Name <- gsub("\\.","_",df$Station.Name)
df$Station.Name <- gsub("-","_",df$Station.Name)


#arranging the names in alphabetical order so its easier to see the differences
df_max_snow_depth <- df_max_snow_depth %>% arrange(Station.Name)

df$Station.Name
df_max_snow_depth$Station.Name

#finding columns that don't match
not_matching_df_max <- which(is.na(match(df_max_snow_depth$Station.Name,df$Station.Name)))
not_matching_df <- which(is.na(match(df$Station.Name,df_max_snow_depth$Station.Name)))

df[not_matching_df,]
df_max_snow_depth[not_matching_df_max,]

#fixing columns one by one
df$Station.Name <- gsub("Chalk_Creek_#1","Chalk_Creek",df$Station.Name)
df$Station.Name <- gsub("Chalk_Creek_#2","Chalk_Creek",df$Station.Name)
df$Station.Name <- gsub("Clear_Creek_#1","Clear_Creek",df$Station.Name)
df$Station.Name <- gsub("Clear_Creek_#2","Clear_Creek",df$Station.Name)
df$Station.Name <- gsub("Lakefork_#1","Lakefork",df$Station.Name)
df$Station.Name <- gsub("Lakefork_#3","Lakefork",df$Station.Name)
df$Station.Name <- gsub("Mosby_Mtn_","Mosby_Mtn",df$Station.Name)
df$Station.Name <- gsub("White_River_#1","White_River",df$Station.Name)
df$Station.Name <- gsub("Widtsoe_#3","Widtsoe",df$Station.Name)
df$Station.Name <- gsub("Payson_R_S_","Payson_R_S",df$Station.Name)

#full joining
df_full <- full_join(df,df_max_snow_depth, by = "Station.Name")

vect_full <- vect(df_full,geom=c("Longitude","Latitude"))
?plot
plot(vect_full, "Value", type="continuous",box=F,buffer=T,xlim=c(-115,-108),ylim=c(36.7,42),
     main="SNOTEL Snow Depth across Utah 2021-2022 Winter")
library(maps)
map(database="state",regions = "Utah",add=T)


#3
par(mfrow = c(1,3), mar = c(1,1,1,1))

#equal interval plots
map(database="state",regions = "Utah")
plot(vect_full,
     type = 'interval',
     breakby = "eqint",
     "Value",
     breaks = 7,
     col = rev(colorspace::sequential_hcl(7)),
     box=F,
     buffer=T,
     xlim=c(-115,-108),ylim=c(36.5,43),
     main= "Equal Interval",
     add=T)


#quantile plots
map(database="state",regions = "Utah")
plot(vect_full,
     type = 'interval',
     breakby = "cases",
     "Value",
     breaks = 7,
     col = rev(colorspace::sequential_hcl(7)),
     box=F,
     buffer=T,
     xlim=c(-115,-108),ylim=c(36.5,43),
     main= "Equal Quantile Breaks",
     add=T)




#standard deviation plots
snow_mean <- mean(vect_full$Value,na.rm=T)
snow_sd <- sd(vect_full$Value,na.rm=T)

breaks = c((snow_mean - 3*snow_sd),
            (snow_mean - 2*snow_sd),
           (snow_mean - snow_sd),
           snow_mean,
            (snow_mean + snow_sd),
           (snow_mean + 2*snow_sd),
           (snow_mean + 3*snow_sd))


map(database="state",regions = "Utah")
plot(vect_full,
     type = 'interval',
     "Value",
     breaks = breaks,
     col = rev(colorspace::diverge_hsv(6)),
     box=F,
     buffer=T,
     xlim=c(-115,-108),ylim=c(36.5,43),
     add=T,
     main = "Standard Deviation")

#4

UT_shape <- vect("./task_7/UT_HUC8/UT_HUC8.shp")

par(mfrow = c(1,1),mar = c(4,4,4,4))
plot(UT_shape,"NAME",legend = F)
text(UT_shape, "NAME", cex=0.35, col='black')


#5
desired_extent <- c(-111.952211,-111.536395,39.915063,40.471646)
extent <- ext(desired_extent)
polygon_extent <- as.polygons(extent)

extent <- as.polygons(ext(c(418618.3, 454529.9, 4418468, 4480543)))
plot(UT_shape)

crs(polygon_extent) <- "EPSG:26912"
#projecting extent to UT_shape crs
extent <- project(polygon_extent,"EPSG:26912")

# Plot the UT_shape
plot(UT_shape)

# Plot the extent
plot(extent, border = "firebrick", lwd = 5, add = TRUE)

# Add legend for extent
legend(x=500000,y=4650000,legend = "Study Area", lty=1,col="red")

#6
above_39_lat <- as.polygons(ext(c(-120,-100,39,60)))
crs(above_39_lat) <- "epsg:4269"
above_39_lat <- project(above_39_lat,UT_shape)

crop_UT_shape <- crop(UT_shape,above_39_lat)
plot(crop_UT_shape, "NAME",legend = F)
text(crop_UT_shape,"NAME",cex = 0.3)


#7
island <- vect("https://raw.githubusercontent.com/mattols/geospat_data/main/AntelopeIsland.geojson")

island <- project(island,UT_shape)


plot(UT_shape[UT_shape$NAME == "Great Salt Lake"], col= "lightblue")
plot(island, add = T,col="white")
text(island, "Antelope Island", cex = 0.3)


#8
HUC4 <- UT_shape
HUC2 <- UT_shape


HUC4$HUC <- substr(UT_shape$HUC,1,nchar(UT_shape$HUC) - 4)
HUC2$HUC <- substr(UT_shape$HUC,1,nchar(UT_shape$HUC) - 6)

HUC2 <- aggregate(HUC2,"HUC")
HUC4 <- aggregate(HUC4,"HUC")

# Plot UT_shape with HUC attribute
plot(UT_shape, "HUC", legend = FALSE)
plot(HUC2, "HUC", add = TRUE, col = "transparent", border = "white", legend = FALSE, lwd = 6, lty = 1)
plot(HUC4, "HUC", add = TRUE, col = "transparent", border = "red", legend = FALSE, lwd = 2.5, lty = 3)

# Create legend for HUC2 and HUC4
legend("topright", legend = c("HUC8","HUC4", "HUC2"), col = c("black","red", "white"), lty = c(1,3, 1), lwd = c(1,2, 4),bg = "lightgrey")
text(HUC4,"HUC",cex = 0.75)

HUC4$NAME

#9

#adding coordinate system to full snotel data vector
crs(vect_full) <- "epsg:4269"

#reprojecting to the same coordinate system as HUC8 data
vect_full <- project(vect_full,UT_shape)

plot(UT_shape,"NAME",legend=F)
plot(vect_full,add=T)


intersected <- intersect(UT_shape,vect_full)

intersect_df <- as.data.frame(intersected)

#plotting each sheds snow depth by elevation plots
intersect_df %>%
  filter(Value != 0) %>%
  ggplot(aes(x = Elevation..ft., y = Value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(5, 90) +
  facet_wrap(~ HUC) +
  theme_bw()


intersect_df <- intersect_df %>% 
  filter(Value!= 0 & is.na(Value) == FALSE)

models <- intersect_df %>% 
  group_by(HUC) %>% 
  do(model = lm(formula = Value~Elevation..ft., data = .))


# I had chatgpt help me with this, I wanted to make it so I could create the plot
# Initialize an empty data frame to store results
coefficients_df <- data.frame(HUC = character(), Intercept = numeric(), Elevation_Coefficient = numeric())

# Loop over each model
for (i in seq_along(models$model)) {
  # Extract coefficients from the current model
  coef_values <- coef(models$model[[i]])
  
  # Extract HUC value
  huc_value <- models$HUC[i]
  
  # Append coefficients to the data frame
  coefficients_df <- bind_rows(coefficients_df, data.frame(HUC = huc_value, 
                                                           Intercept = coef_values[1], 
                                                           Elevation_Coefficient = coef_values[2]))
}

# Print the resulting data frame
print(coefficients_df)

# Convert HUC column in coefficients_df to character
coefficients_df$HUC <- as.character(coefficients_df$HUC)

# Merge the coefficients data frame with the UT_shape vector data
merged_data <- merge(UT_shape, coefficients_df, by = "HUC")

merged_data$Elevation_Coefficient <- round(merged_data$Elevation_Coefficient,4)
merged_data$Intercept <- round(merged_data$Intercept,2)
# Plot merged_data
plot(merged_data, legend = FALSE,main = "Slope and intercept of snow depth values in a linear model compared to elevation")
# Add text with "slope" before the elevation coefficient values
text(merged_data, labels = paste("slope:", merged_data$Elevation_Coefficient), cex = 0.4)
text(merged_data, labels = paste("Intercept:", merged_data$Intercept), cex = 0.4,pos = 3)


intersect_df %>% 
  lm(formula = Value~Elevation..ft.)

summary(model)

#10
HUCs <- unique(intersect_df$HUC)

# Initialize an empty list to store subsets
subset_list <- list()

huc_data <- data.frame(
  HUC = character(),
  Station_Count = integer(),
  Max_Snow_Depth = numeric(),
  Mean_Elevation = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each unique HUC value
for (i in unique(intersect_df$HUC)) {
  # Create a subset of intersect_df for the current HUC
  subset_df <- intersect_df[intersect_df$HUC == i, ]

    cat(paste0("Number of stations for HUC ", i, ": ",nrow(subset_df),"\n"))
    cat(paste0("Max snow depth for HUC ", i, ": ",max(subset_df$Value),"\n"))
    cat(paste0("Mean elevation for HUC ",i,": ",round(mean(subset_df$Elevation..ft.),2),"\n","\n"))
   
     # Calculate the number of stations
    station_count <- nrow(subset_df)
    
    # Calculate the max snow depth
    max_snow_depth <- max(subset_df$Value)
    
    # Calculate the mean elevation
    mean_elevation <- round(mean(subset_df$Elevation..ft.), 2)
    
    # Add a row to the data frame
    huc_data <- rbind(huc_data, data.frame(HUC = i, Station_Count = station_count, Max_Snow_Depth = max_snow_depth, Mean_Elevation = mean_elevation))
}

huc_final <- merge(UT_shape,huc_data)
par(mfrow= c(2,1))
plot(huc_final,"Max_Snow_Depth",type="continuous")
text(huc_final,"Station_Count")
plot(huc_final,"Mean_Elevation",type="continuous")
text(huc_final,"Max_Snow_Depth")
