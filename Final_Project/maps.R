# Install and load required packages
install.packages(c("leaflet", "sf", "rgdal"))

# Load the libraries
library(leaflet)
library(sf)
library(rgdal)

# Set the center and zoom level for Utah
utah_location <- c(lat = 39.3200, lon = -111.8883)
zoom_level <- 7

# Set the UTM coordinates for Marysvale
marysvale_extent_utm <- list(easting_min = 360000, easting_max = 390000, northing_min = 4235000, northing_max = 4265000)

# Create an sf object with a Point geometry for UTM coordinates
marysvale_points_utm <- st_sfc(st_point(c(marysvale_extent_utm$easting_min, marysvale_extent_utm$northing_min)),
                               st_point(c(marysvale_extent_utm$easting_max, marysvale_extent_utm$northing_max))) %>%
  st_set_crs("+proj=utm +zone=12 +datum=WGS84")

# Transform UTM coordinates to latitude and longitude
marysvale_points_utm <- st_set_crs(marysvale_points_utm, "+proj=utm +zone=12 +datum=WGS84")
marysvale_points_lonlat <- st_transform(marysvale_points_utm, "+proj=longlat +datum=WGS84")

# Extract the transformed coordinates
marysvale_extent <- st_bbox(marysvale_points_lonlat)

# Create a leaflet map with HOT tile layer
utah_map <- leaflet() %>%
  setView(lng = utah_location["lon"], lat = utah_location["lat"], zoom = zoom_level) %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addRectangles(
    lng1 = marysvale_extent[1], lat1 = marysvale_extent[2],
    lng2 = marysvale_extent[3], lat2 = marysvale_extent[4],
    fillColor = "transparent",
    color = "red",
    weight = 3
  )

# Show the map
utah_map
