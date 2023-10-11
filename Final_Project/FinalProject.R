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
library(plotly)
library(cowplot)

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

subl <- l[c(8:57,77:224)] #removing non working bands

e <- as(extent(327500, 332500, 4150000, 4155000), 'SpatialPolygons') #setting extent
x <- rast(subl) #creating raster from list
x<- crop(x,e) #cropping by extent
df<- as.data.frame(x,xy=TRUE,cells=TRUE,na.rm=TRUE) #creating dataframe
  
r <- df

colnames(r)<- r %>% 
  gsub(pattern = "^[^_]*_([^_]*).*",
       replacement = "\\1", x=names(r))#getting rid of everything before the first underscore and after the second one

r <- r %>% 
pivot_longer(cols = starts_with("B"), #moving all bands into one column
             names_to = "Bands",
             values_to = "DN") %>% 
  rename(easting = x,northing = y)




#try this but first make list of TIF files as dataframes individually
df <- subl %>% 
  lapply(raster) %>% 
  lapply(rasterToPoints)


dat <- map(files,function)

dfreduce <- reduce(df,cross_join)

#---------------------------------------------------------------------------------------------------
#one that works but takes longer
# :(
# r <- l %>% 
#   lapply(raster) %>%  #rasterizing all files
#   lapply(crop, e) %>% #cropping all files in list by extent e
#   stack() %>% #stacking all rasters
#   rasterToPoints()  #converting stack to table with x y and DN
# as.data.frame()
#---------------------------------------------------------------------------------------------------

#combining band information with raster information
r <- full_join(r,Hyperion_Bands) #combing dataframes
r <- na.omit(r) # removing all rows with NA
r <- clean_names(r) #cleaning names
#---------------------------------------------------------------------------------------------------
#plotting
#plotting by cell#
subr = r %>% 
  filter(cell == 12447)
subr3 = r %>% 
  filter(cell == 12439)

#1-27722 amount of cells

p <- subr %>% 
  ggplot(aes(x=wavelength_nm,y=dn)) +
  geom_line()

p2 <- subr3 %>% 
  ggplot(aes(x=wavelength_nm,y=dn)) +
  geom_line()

plot_grid(p, p2, labels = "AUTO")


#plotting by band#
subr2 <- r %>% 
  filter(bands == "B025")

map <- subr2 %>% 
  ggplot() +
  geom_tile(aes(x = easting, y = northing, fill = dn, text = cell)) +
  theme_classic()
ggplotly(map, tooltip = c("cell","easting","northing"))





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
