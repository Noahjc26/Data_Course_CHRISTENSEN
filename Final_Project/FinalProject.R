library(raster)
# library(hyperSpec)
# library(devtools)
library(terra)
library(tidyverse)
# library(gdalUtilities)
library(janitor)
# library(rgdal)
library(plotly)
library(cowplot)
library(prismatic)
library(stringr)


#[1] reading in csv of hyperion band information
#[2] removing nm in wavelength column
#[3] removing FWHM: in wavelength column
#[4] separating Wavelength and FWHM into their own columns
#[5] separating VNIR and SWIR from band names
#[6] removing temp columns 
#[7] reading in metadata
#[8] reading in bands

Hyperion_Bands <- read.csv("Hyperion_Bands_Wavelengths.csv") %>%  #[1]
  select(-c(X,X.1,X.2,X.3)) #removing random extra columns
Hyperion_Bands <- Hyperion_Bands[-(199:235),] #removing random extra rows

Hyperion_Bands$Wavelength <- gsub("nm","",Hyperion_Bands$Wavelength)    #[2]
Hyperion_Bands$Wavelength <- gsub("FWHM:","",Hyperion_Bands$Wavelength) #[3]

Hyperion_Bands <- Hyperion_Bands %>% 
  as.data.frame() %>% 
  separate(Wavelength,into = c("Wavelength_nm","FWHM_nm"),sep=",",convert = TRUE) %>% #[4]
  separate(Description,into = c("Description", "Temp", "Temp2"),sep=" ") %>%          #[5]
  rename("Bands" = "Band_Name")

Hyperion_Bands <- subset(Hyperion_Bands, select = -c(Temp, Temp2)) #[6]


md <- read_lines("../../Hyperion/L1T/EO1H0380342005105110KF_1T/EO1H0380342005105110KF_MTL_L1T.TXT") #[7]

#setting up variables for reflectance equation
julian_day <- as.numeric(word(md[18], 8))
d <- (1-0.01672*cos(0.9865*(julian_day-4)))#earth sun distance in astronomical distance
sun_elevation <- as.numeric(word(md[300],7))
s <- (90-sun_elevation) #solar zenith angle indegrees


l <- list.files(path="../../Hyperion/L1T/EO1H0380342005105110KF_1T/", #[8]
                pattern='TIF$',
                full.names=TRUE)

e <- as(extent(327500, 332500, 4150000, 4155000), 'SpatialPolygons') #setting extent

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


#---------------------------------------------------------------------------------------------------
#UGS interactive map
UGS <- rast("../../UGS/StateOfUtah250k.tif")
head(describe("../../UGS/StateOfUtah250k.tif"),n=55)
UGS
ext(UGS) #checking extent of UGS
e <- as(extent(314700, 352800, 4110000, 4219800), 'SpatialPolygons')
croppedUGS <- crop(UGS, e)
#croppedUGS <- aggregate(croppedUGS,2) #makes pixels 2 times are large
plot(croppedUGS)

saveRDS()
#---------------------------------------------------------------------------------------------------
