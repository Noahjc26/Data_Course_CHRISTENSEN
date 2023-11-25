library(tidyverse)
library(plotly)

#reading in hyperion info
hyperion_info <- read_rds("./cleaned_hyperion_band_info.rds")

#reading in landsat info
landsat_info <- read_rds("./landsat_8_band_info.rds")

#reading in aster info
aster_info <- read_rds("./aster_band_info.rds")


part <- full_join(hyperion_info,aster_info)

full <- full_join(part,landsat_info)

head(full)


# Define a color palette
my_colors <- c("#E41A1C", "#117EB8", "#FFC20A", "#984EA3")

# Plotting the boxes with different y-axis limits, prettier colors, and borders
 ggplot(full, aes(x = Wavelength_nm, xmin = lower, xmax = upper, ymin = as.numeric(factor(Satellite_Name)) - 0.4, ymax = as.numeric(factor(Satellite_Name)) + 0.4, fill = Satellite_Name)) +
  geom_rect(color = "black", linewidth = 0.3, alpha = .6) +
  labs(title = "Satellites FWHM",
       x = "Wavelength (nm)",
       y = "") +
  scale_fill_manual(values = my_colors) +
   theme_minimal() +
   theme(axis.text.y = element_blank())



# Plotting the boxes with different y-axis limits, prettier colors, and borders
p <- ggplot(full, aes(x = Wavelength_nm, xmin = lower, xmax = upper, ymin = as.numeric(factor(Satellite_Name)) - 0.4, ymax = as.numeric(factor(Satellite_Name)) + 0.4, fill = Satellite_Name)) +
  geom_rect(color = "black", size = 0.4, alpha = 0.8) +
  
  # Adding labels
  labs(title = "Satellites FWHM",
       x = "Wavelength (nm)",
       y = "",
       fill = "Satellite") +
  
  # Set color palette
  scale_fill_manual(values = my_colors, labels = c("ASTER","Hyperion","Landsat 8")) +
  
  # Minimal theme with y-axis grid and numbers off
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  # Reorder legend items
  guides(fill = guide_legend(reverse = TRUE))

ggplotly(p, tooltip = c(full$Bands))

ggsave(filename = "../../Noahjc26.github.io/Media/satellites_FWHM.png",plot = p)












# Load the ggplot2 library
library(ggplot2)

# Generate dummy data for demonstration
wavelengths <- seq(400, 700, length.out = 100)
transmission <- dnorm(wavelengths, mean = 550, sd = 30)

# Normalize the values so that the area under the curve sums to 100%
transmission <- transmission / sum(transmission) * 2100

# Create a data frame
data <- data.frame(Wavelength = wavelengths, Transmission = transmission)

# Plot the bell curve and vertical lines
pp <- ggplot(data, aes(x = Wavelength, y = Transmission)) +
  geom_line() +
  geom_vline(xintercept = c(519, 581), linetype = "dashed", color = "red") +
  labs(title = "Visualizing FWHM",
       x = "Wavelength",
       y = "% Transmission") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
  ) +
  ylim(0,100)
pp
ggsave("../../Noahjc26.github.io/Media/visualizing_FWHM.png",pp)







