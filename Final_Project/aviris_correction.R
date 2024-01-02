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
aviris2 <- rast("../../AVIRIS/1999/f991017t01p02_r11c/f991017t01p02_r11rdn_a_img")

#rectifying (this takes a while)
aviris <- rectify(aviris)


aviris %>% 
plotRGB(50,20,10)

aviris2 <- rectify(aviris)
plot(values(aviris[[1]]))




