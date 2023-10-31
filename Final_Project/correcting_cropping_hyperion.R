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
md <- read_lines("../../Hyperion/L1T/EO1H0380342005105110KF_1T/EO1H0380342005105110KF_MTL_L1T.TXT")

#setting up variables for reflectance equation
julian_day <- as.numeric(word(md[18], 8)) #getting julian day
d <- (1-0.01672*cos(0.9865*(julian_day-4))) #earth sun distance in astronomical distance
sun_elevation <- as.numeric(word(md[300],7)) #getting sun elevation
s <- (90-sun_elevation)  #solar zenith angle in degrees

#reading in bands
l <- list.files(path="../../Hyperion/L1T/EO1H0380342005105110KF_1T/",
                pattern='TIF$',
                full.names=TRUE)

#setting extent
e <- as(extent(332500, 337500, 4170000, 4175000), 'SpatialPolygons')

#removing non working bands
r <- rast(l[c(8:57,77:224)])

#cropping r
r <- crop(r,e)

#adding value in new column based on Hyperion_Bands$Description
vect <- if_else(Hyperion_Bands$Description == "VNIR",40,80)

#adding vect as new column
Hyperion_Bands$Rad_Conv = vect

#correcting to surface reflectance
Surf_Reflectance = (pi*(r/Hyperion_Bands$Rad_Conv)*d^2)/(cos(s*pi/180)*Hyperion_Bands$Irradiance)

#saving as RDS
saveRDS(Surf_Reflectance,"./corrected_EO1H0380342005105110KF_1T.rds")








#performing everything but changing it to a dataframe first
df <- l[c(8:57,79,83:119,133:164,183:184,188:220)] %>% #removing non working bands
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
  filter(bands == "B025") 

map <- map %>% 
  ggplot() +
  geom_raster(aes(x = easting, y = northing, fill = reflectance, text = cell)) +
  theme_classic()

ggplotly(map, tooltip = c("cell","easting","northing"))


raster <- (rast(l))
crop(r)
plot()
ra <- stack(ra)
plot(ra)
tmap_leaflet(                                                      
  tm_shape(as(raster, "Raster")) + # what sf to use for creating a map 
    tm_raster())

