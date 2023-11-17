#reading in hyperion info
hyperion_info <- read_rds("./cleaned_hyperion_band_info.rds")

#reading in landsat info
landsat_info <- read_rds("./landsat_8_band_info.rds")

#reading in aster info
aster_info <- read_rds("./aster_band_info.rds")


part <- full_join(hyperion_info,aster_info)

full <- full_join(part,landsat_info)


ggplot(hyperion_info, aes(x = Wavelength_nm, y = lower, ymax = upper)) +
  geom_rect(aes(xmin = Bands, xmax = Bands, ymin = lower, ymax = upper), alpha = 0.5) +
  labs(title = "Square Plot of Lower and Upper Bounds", x = "Category", y = "Value") +
  theme_minimal()
