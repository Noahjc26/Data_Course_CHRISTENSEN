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

#rasterizing aviris imagery
aviris2 <- rast("/Users/noah/Desktop/AVIRIS/fish_lake/f100825t01p00r12rdn_b/f100825t01p00r12rdn_b_sc01_ort_img")

#rectifying (this takes a while)
aviris <- rectify(aviris)

writeRaster(aviris,"../../AVIRIS/fish_lake/f100825t01p00r12rdn_b_sc01_ort_img_corrected.tif")
head(aviris)

aviris %>% 
plotRGB(50,20,10)

aviris2 <- rectify(aviris)
plot(values(aviris[[1]]))




