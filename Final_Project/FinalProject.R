library(raster)
library(hyperSpec)
library(devtools)
library(terra)
library(tidyverse)
library(gdalUtilities)
library(janitor)
library(dplyr)
library(zoom)
library(unix)
library(rgdal)
library(janitor)
library(tmap)

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
  separate(Wavelength,into = c("Wavelength_nm","FWHM_nm"),sep=",",convert = TRUE) %>% 
  separate(Description,into = c("Description", "Temp", "Temp2"),sep=" ") %>% 
  rename("Bands" = "Band_Name")
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
#reading in bands to r
l <- list.files(path="../../Hyperion/L1T/EO1H0380342005105110KF_1T/",
                pattern='TIF$',
                full.names=TRUE)

e <- as(extent(325000, 330000, 4100000, 4150000), 'SpatialPolygons') #setting extent
x <- rast(l)
x<- crop(x,e)
xd<- as.data.frame(x,xy=TRUE,cells=TRUE)


#cropping imagery
rast()
ll <- rast(l)
p <- terra::as.points(l)
xx <- terra::extract(p)
head(p)
a_df <- as.data.frame(r, na.rm = TRUE,cells=TRUE) 

x = as.data.frame(rasterToPoints(ll,spatial = TRUE))

cellFromRowCol(l)
xyFromCell(l)
extract(l,xy)
r <- l %>% 
  stack() %>% 
  rasterToPoints()


x <- l %>% 
stack()
xx  <- crop(x, e)
xxx<- rasterToPoints(xx)

#takes forever :(
r <- l %>% 
  lapply(raster) %>%  #rasterizing all files
  lapply(crop, e) %>% #cropping all files in list by extent e
  stack() %>% #stacking all rasters
  rasterToPoints() %>% #converting stack to table with x y and DN
  as.data.frame()

r <- xd

brick
colnames(r)<- r %>% 
  gsub(pattern = "^[^_]*_([^_]*).*",
       replacement = "\\1", x=names(r))#getting rid of everything before the first underscore and after the second one

names(r)[1] <- "Easting" 
names(r)[2] <- "Northing"

r <- r %>% 
pivot_longer(cols = starts_with("B"), #moving all bands into one column
             names_to = "Bands",
             values_to = "DN")

#---------------------------------------------------------------------------------------------------
#combining band information with raster information
r <- full_join(r,Hyperion_Bands) #combing dataframes
r <- na.omit(r) # removing all rows with NA

#---------------------------------------------------------------------------------------------------
#plotting
print(lapply(r,class))

r <- clean_names(r)

subr = r %>% 
  filter(northing > '4145010' & northing < '4145025'
         & easting < '325176.9' & easting > '325176.7')


subr %>% 
  ggplot(aes(x=wavelength_nm,y=dn)) +
  geom_line()



rrr %>% 
ggplot() +
  geom_raster(aes(x = Easting, y = Northing, fill = B016)) +
  theme_classic()

#---------------------------------------------------------------------------------------------------
#testing with one TIF
B016 <- rast("../../Hyperion/L1T/EO1H0380342005105110KF_1T/EO1H0380342005105110KF_B016_L1T.TIF")
plot(B016)
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

saveRDS()
#---------------------------------------------------------------------------------------------------
#using tmap converting rast from terra to raster
library(tmap)

r_raster <- raster::raster(UGSa)

tm_shape(r_raster) +
  tm_raster()
