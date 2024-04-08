# Install and load required packages
install.packages(c("leaflet", "sf", "rgdal"))

# Load the libraries
library(leaflet)
library(sf)
library(rgdal)

# Set the center and zoom level for Utah
utah_location <- c(lat = 39.3200, lon = -111.8883)
zoom_level <- 7

# Set the UTM coordinates for the larger extent
larger_extent_utm <- list(easting_min = 380000, easting_max = 420500, northing_min = 4220000, northing_max = 4260000)

# Set the UTM coordinates for Marysvale
marysvale_extent_utm <- list(easting_min = 402000, easting_max = 404500, northing_min = 4246000, northing_max = 4248000)

# Create an sf object with a Point geometry for UTM coordinates
larger_extent_points_utm <- st_sfc(st_point(c(larger_extent_utm$easting_min, larger_extent_utm$northing_min)),
                                   st_point(c(larger_extent_utm$easting_max, larger_extent_utm$northing_max))) %>%
  st_set_crs("+proj=utm +zone=12 +datum=WGS84")

marysvale_points_utm <- st_sfc(st_point(c(marysvale_extent_utm$easting_min, marysvale_extent_utm$northing_min)),
                               st_point(c(marysvale_extent_utm$easting_max, marysvale_extent_utm$northing_max))) %>%
  st_set_crs("+proj=utm +zone=12 +datum=WGS84")

# Transform UTM coordinates to latitude and longitude
larger_extent_points_utm <- st_set_crs(larger_extent_points_utm, "+proj=utm +zone=12 +datum=WGS84")
marysvale_points_utm <- st_set_crs(marysvale_points_utm, "+proj=utm +zone=12 +datum=WGS84")

larger_extent_points_lonlat <- st_transform(larger_extent_points_utm, "+proj=longlat +datum=WGS84")
marysvale_points_lonlat <- st_transform(marysvale_points_utm, "+proj=longlat +datum=WGS84")

# Extract the transformed coordinates
larger_extent <- st_bbox(larger_extent_points_lonlat)
marysvale_extent <- st_bbox(marysvale_points_lonlat)

# Create a leaflet map with HOT tile layer
utah_map <- leaflet() %>%
  setView(lng = utah_location["lon"], lat = utah_location["lat"], zoom = zoom_level) %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addRectangles(
    lng1 = larger_extent[1], lat1 = larger_extent[2],
    lng2 = larger_extent[3], lat2 = larger_extent[4],
    fillColor = "transparent",
    color = "blue",
    weight = 3
  ) %>%
  addRectangles(
    lng1 = marysvale_extent[1], lat1 = marysvale_extent[2],
    lng2 = marysvale_extent[3], lat2 = marysvale_extent[4],
    fillColor = "transparent",
    color = "red",
    weight = 3
  )

# Show the map
utah_map

# Install and load the mapview package
library(mapview)
# Install webshot package
install.packages("webshot")

# Install PhantomJS
webshot::install_phantomjs()

# Save the Leaflet map as an image
mapshot(utah_map, file = "./images/utah_zoomed.png", width = 11.1, height = 11.4, dpi = 1000)
ggsa