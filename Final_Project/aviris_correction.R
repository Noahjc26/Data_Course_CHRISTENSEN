# Load necessary libraries
library(raster)
library(sf)
library(rgdal)
library(gdal)

# Replace with the actual file path of your geometric correction file
geo_file <- "../../AVIRIS/1999/f991017t01p02_r11c/f991017t01p02_r11.geo"

geo_data <- read.table(geo_file, header = FALSE)

# Extract relevant columns
spatial_info <- geo_data[, c(1, 2, 5)]

# Apply geometric correction (example - modify based on your correction formula)
corrected_coords <- spatial_info[, 1] * 1000  # Convert milliradians to radians (example)
# Add other correction calculations based on your specific formula

# Create a new raster with corrected spatial information
aviris_corrected <- stack("../../AVIRIS/1999/f991017t01p02_r11c/f991017t01p02_r11rdn_a_img")  # Replace with your AVIRIS image file

# Replace "EPSG:XXXX" with the appropriate EPSG code for your data
crs(aviris_corrected) <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"

# Assuming corrected_coords contains the corrected spatial information
# Assuming corrected_coords contains the corrected spatial information
extent(aviris_corrected) <- c(min(38.4955), max(38.525), min(-112.2985), max(-112.237333))

res(aviris_corrected) <- c(spatial_info[1, 1], spatial_info[1, 2])  # Use the correct pixel size from the *.geo file

plot(aviris_corrected[[1]])

st_drivers()
# Plot the raster to visualize the extent
plot(aviris_geo$geometry)


# Load the raster layer
aviris <- raster("../../AVIRIS/1999/f991017t01p02_r11c/f991017t01p02_r11rdn_a_img")

# Display one of the raster bands for visualization
plot(aviris[[50]])

# Given start and end latitude and longitude coordinates
pilot_st_lat <- 38.4955
pilot_end_lat <- 38.525
pilot_st_long <- -112.2985
pilot_end_long <- -112.237333

# Set the spatial extent based on the provided coordinates
extent_info <- extent(pilot_st_long, pilot_end_long, pilot_st_lat, pilot_end_lat)

# Create an empty raster with the specified extent
empty_raster <- raster(ext = extent_info)

# Replace "your_image.tif" with the actual file path of your AVIRIS image
aviris_raster <- raster("../../AVIRIS/1999/f991017t01p02_r11c/f991017t01p02_r11rdn_a_img", template = empty_raster)

# Plot the raster to visualize the extent
plot(aviris_raster)
