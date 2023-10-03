library(BiocManager)
library(raster)
library(rgdal)
library(hyperSpec)
library(devtools)
library(terra)
library(tidyverse)
library(gdalUtilities)
library(janitor)
library(dplyr)
library(zoom)
library(dplyr)
library(unix)

#---------------------------------------------------------------------------------------------------
#reading in csv of hyperion band information
Hyperion_Bands <- read.csv("Hyperion_Bands_Wavelengths.csv")

#removing nm in wavelength column
Hyperion_Bands$Wavelength <- gsub("nm","",Hyperion_Bands$Wavelength)

#removing FWHM: in wavelength column
Hyperion_Bands$Wavelength <- gsub("FWHM:","",Hyperion_Bands$Wavelength) 

#Separating Wavelength and FWHM into their own columns
Hyperion_Bands <- Hyperion_Bands %>% 
  as.data.frame() %>% 
  separate(Wavelength,into = c("Wavelength_nm","FWHM_nm"),sep=",") %>% 
  separate(Description,into = c("Description", "Temp", "Temp2"),sep=" ") 
#separating VNIR and SWIR from band names

#removing temp columns
Hyperion_Bands <- subset(Hyperion_Bands, select = -c(Temp, Temp2))


#---------------------------------------------------------------------------------------------------
#reading in metadata
md <- read_lines("../../Hyperion/L1T/EO1H0380342005105110KF_1T/EO1H0380342005105110KF_MTL_L1T.TXT")
#[298] Sensor look angle
#[299] sun azimuth
#[300] sun elevation
#NIR (1 to 70) /40 and SWIR (71 to 242) /80


#---------------------------------------------------------------------------------------------------
#reading in bands to f
f <- list.files(path="../../Hyperion/L1T/EO1H0380342005105110KF_1T/",
                pattern='TIF$',
                full.names=TRUE)

#rasterizing all files in f
r <- lapply(f, raster)

#cropping imagery
e <- as(extent(310000, 320000, 4150000, 4200000), 'SpatialPolygons') #setting extent
rr <- lapply(r, crop, e) #cropping all files in list by extent e

#stacking all rasters
df <- stack(rr)

#converting stack to table with x y and DN
df_all <- rasterToPoints(df)
df_all_fr <- as.data.frame(df_all)

df_all_fr %>%
  rename(Easting = x,
         Northing = y)

#---------------------------------------------------------------------------------------------------
#testing with one TIF
B016 <- rast("../../Hyperion/L1T/EO1H0380342005105110KF_1T/EO1H0380342005105110KF_B016_L1T.TIF")

#cropping imagery
e <- as(extent(310000, 320000, 4150000, 4200000), 'SpatialPolygons')
B016 <- crop(B016, e)

#turning rast into dataframe
B016 = as.data.frame(B016, xy=TRUE,na.rm=TRUE)


# Function to convert Radiance to TOA Reflectance
# PTOA = (pi*L*d^2) / E*cos(theta)
# PTOA (top of atmosphere reflectance)
# Radiance (at satellite radiance)
# d (Earth Sun distance correction factor)

# E (extraterrestrial irradiance)
# theta (angle of solar incidence)

B016r <- B016 %>%
  add_column(Band_Name = "B016") %>% 
  rename(DN = EO1H0380342005105110KF_B016_L1T,
         Easting = x,
         Northing = y) %>% 
  mutate(Radiance = DN/40) #DN to radiance
 # mutate(Reflectance = (pi*Radiance*)) #Radiance to reflectance


total <- merge(B016a,Hyperion_Bands,by="Band_Name")


#plotting
ggplot() +
  geom_raster(data=B016r,aes(x = Easting, y = Northing, fill = Radiance)) +
  scale_color_viridis() +
  theme_classic()



#---------------------------------------------------------------------------------------------------
#UGS interactive map
UGS <- rast("../../UGS/StateOfUtah250k.tif")
head(describe("../../UGS/StateOfUtah250k.tif"),n=55)
UGS
ext(UGS) #checking extent of UGS
e <- as(extent(310000, 320000, 4150000, 4200000), 'SpatialPolygons')
croppedUGS <- crop(UGS, e)
#croppedUGS <- aggregate(croppedUGS,2) #makes pixels 2 times are large
plot(croppedUGS)
df <- as.data.frame(croppedUGS,xy=TRUE,na.rm=TRUE)


#---------------------------------------------------------------------------------------------------
#using tmap converting rast from terra to raster
library(tmap)

r_raster <- raster::raster(UGSa)

tm_shape(r_raster) +
  tm_raster()
