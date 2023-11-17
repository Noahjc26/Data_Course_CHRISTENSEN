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
library(leaflet)


surf_reflectance <- rast("../../Hyperion/corrected_EO1H0380332004336110PZ_1T.tif")

surf_reflectance %>% 
plotRGB(31,20,10,stretch = "lin")

e <- as(extent(330000, 342000, 4232000, 4245000), 'SpatialPolygons')

cropped <- crop(surf_reflectance,e)

cropped %>% 
  plotRGB(60,20,30,stretch = "lin")


beaver <- rast("../../Quads/Beaver_NW_Quad/ofr-729/BeaverNW__GeoTiff_NAD27.tif")
beaver
plotRGB(beaver,1,2,3)


#making Milford_quad have the same crs as surf_reflectance
test <- project(beaver,crs(cropped))

beav <- crop(test,e)

plotRGB(beav,1,2,3)
plotRGB(cropped,31,20,10,stretch = "lin")


# Arrange the plots side by side
plot_grid(p1, p2, ncol = 2)


med <- read_lines("../../Hyperion/mineral_mountain/EO1H0380332004336110PZ_1T/EO1H0380332004336110PZ_MTL_L1T.TXT")
med

min <- readRDS("./hyperion_mineral_signatures_cleaned.rds")
unique(min$mineral)


beaver_minerals <- subset(min, mineral == c("quartz","calcite","dolomite"))
unique(min$mineral)
long <- beaver_minerals %>% 
  pivot_longer(starts_with("B"),
               names_to = "Bands")

long %>% 
  ggplot(aes(x=Bands,y=value,color=mineral)) +
  geom_point(size=.5)

#reading in hyperion band info
band_info <- read_rds("./hyperion_band_info.rds")

full <- full_join(band_info,long)

#. traps function
trapz <- function(x, y) {
  sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
}

# Define the absorption feature range
absorption_range <- c(1400, 1600)  # Adjust this based on your spectral data

# Function to calculate the area under the curve (AUC) within the absorption feature range
calculate_auc <- function(mineral_spectrum) {
  spectrum_subset <- subset(mineral_spectrum, Wavelength_nm >= absorption_range[1] & Wavelength_nm <= absorption_range[2])
  auc <- trapz(spectrum_subset$Wavelength_nm, spectrum_subset$value)
  return(auc)
}


# Apply the function to calculate AUC for each mineral
mineral_auc <- full %>%
  group_by(mineral) %>%
  summarise(AUC = calculate_auc(.))

# Print the AUC for each mineral
print(mineral_auc)

