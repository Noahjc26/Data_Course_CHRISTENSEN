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

#reaading in hyperion band info
Hyperion_Bands <- readRDS("./cleaned_hyperion_band_info.rds")

# reading in metadata
md <- read_lines("../../Hyperion/sevier_lake/EO1H0380332014325110PZ_1T/EO1H0380332014325110PZ_MTL_L1T.TXT")

#setting up variables for reflectance equation
julian_day <- as.numeric(word(md[18], 8)) #getting julian day
d <- (1-0.01672*cos(0.9865*(julian_day-4))) #earth sun distance in astronomical distance
sun_elevation <- as.numeric(word(md[300],7)) #getting sun elevation
s <- (90-sun_elevation)  #solar zenith angle in degrees

#reading in bands
l <- list.files(path="../../Hyperion/sevier_lake/EO1H0380332014325110PZ_1T/",
                pattern='TIF$',
                full.names=TRUE)

# #setting extent
# e <- as(extent(332500, 337500, 4170000, 4175000), 'SpatialPolygons')

#removing non working bands
r <- rast(l[c(8:57,77:224)])

#correcting to surface reflectance
Surf_Reflectance = (pi*(r/Hyperion_Bands$Rad_Conv)*d^2)/(cos(s*pi/180)*Hyperion_Bands$Irradiance)

#splitting names
names = strsplit(names(Surf_Reflectance),"_")

#getting second name for all layers
vect <- lapply(1:nlyr(Surf_Reflectance), function(x) names[[x]][2])

#setting names as vect we just created
names(Surf_Reflectance) = unlist(vect)

# Creating a logical SpatRaster where 1 indicates that all bands are 0
all_bands_zero <- all(Surf_Reflectance == 0, na.rm = TRUE)

# Setting the cells with all bands equal to 0 to NA
Surf_Reflectance[all_bands_zero] <- NA

#writing as a tif file
<<<<<<< HEAD
writeRaster(Surf_Reflectance, filename = "../../Hyperion/corrected_EO1H0380332014325110PZ_1T.tif")
=======
writeRaster(Surf_Reflectance, filename = "./corrected_EO1H0380332014325110PZ_1T.tif")
>>>>>>> 7cdc7f5 (updating final project)


plot(Surf_Reflectance)





#performing everything but changing it to a dataframe first
df <- l[c(8:57,77:224)] %>% #removing non working bands
  rast() %>%  #creating raster from list
  crop(e) %>% #cropping by extent
  as.data.frame(xy=TRUE,cells=TRUE,na.rm=TRUE) #creating dataframe

colnames(df) <- df %>% 
  gsub(pattern = "^[^_]*_([^_]*).*",
       replacement = "\\1", x=names(df)) #getting rid of everything before the first underscore and after the second one

df <- df %>%   
  pivot_longer(cols = starts_with("B"), #moving all bands into one column
               names_to = "Bands",
               values_to = "DN") %>%
  rename(easting = x,northing = y)

df <- full_join(df,Hyperion_Bands) %>% 
  na.omit(df) %>%  # removing all rows with NA
  clean_names() #cleaning names

#calculating radiance
df <- df %>%
  mutate(radiance = case_when(description == "VNIR" ~ dn / 40,
                              description == "SWIR" ~ dn / 80))

#calculating reflectance
df <- df %>% 
  mutate(reflectance = (pi*radiance*d^2)/(cos(s*pi/180)*df$irradiance))

#plotting by cell#
p = df %>% 
  filter(cell == 6000)
p2 = df %>% 
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

map <- map %>% 
  ggplot() +
  geom_raster(aes(x = easting, y = northing, fill = reflectance, text = cell)) +
  theme_classic()

ggplotly(map, tooltip = c("cell","easting","northing"))

