# Load the libraries
library(ggmap)
library(ggplot2)

# Set your Google Maps API key
api_key <- "AIzaSyBTnsCTOGzinIU-xH48iHEzJdB3XaEdAmk"

# Register the API key
register_google(key = api_key)

# Set the center and zoom level for Utah
utah_location <- c(lon = -111.8883, lat = 39.3200)
zoom_level <- 7

# Get the map data using get_map
utah_map <- get_map(location = utah_location, zoom = zoom_level, maptype = "terrain", source = "google")

# Plot the map using ggmap and ggplot2
ggmap(utah_map) +
  theme_void() +
  ggtitle("Map of Utah")
