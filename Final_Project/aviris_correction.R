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
aviris <- rast("../../AVIRIS/1999/f991017t01p02_r11c/f991017t01p02_r11rdn_a_img")

plot(aviris[[50]])


info <- read_lines("../../AVIRIS/1999/f991017t01p02_r11c/f991017t01p02r11.info")


# Given start and end latitude and longitude coordinates
pilot_st_lat <- 38.4955
pilot_end_lat <- 38.525
pilot_st_long <- -112.2985
pilot_end_long <- -112.237333

# Calculate extent
extent_min_x <- min(pilot_st_long, pilot_end_long)
extent_max_x <- max(pilot_st_long, pilot_end_long)
extent_min_y <- min(pilot_st_lat, pilot_end_lat)
extent_max_y <- max(pilot_st_lat, pilot_end_lat)

# Create SpatialPolygons object representing the extent
library(sp)
extent_polygon <- SpatialPolygons(
  list(
    Polygons(
      list(Polygon(cbind(c(extent_min_x, extent_max_x, extent_max_x, extent_min_x),
                         c(extent_min_y, extent_min_y, extent_max_y, extent_max_y)))),
      ID = "1"
    )
  )
)

# Apply the extent to the stacked raster
aviris_with_extent <- setExtent(aviris, extent_polygon)


plotRGB(aviris_with_extent,200,20,25)


