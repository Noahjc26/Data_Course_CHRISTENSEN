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
library(hyperSpec)

landsat_band_info <- readRDS("./landsat_8_band_info.rds")

landsat_band_info <- landsat_band_info %>% 
  filter(Bands != 8) %>% 
  filter(Bands != 9)

list <- list.files("../../usgs_spectral_library/usgs_splib07 (1)/ASCIIdata/ASCIIdata_splib07b_rsLandsat8/ChapterM_Minerals/",
                   full.names = TRUE)

#list applying as csv and into a data frame
minerals <- list %>%
  lapply(read.csv) %>%
  as.data.frame() %>%
  clean_names() %>%
  mutate(Bands = row_number())

#removing everything before first underscore
colnames(minerals) = sub(".*?_", "", colnames(minerals))

#removing everything before first underscore
colnames(minerals) = sub(".*?_", "", colnames(minerals))

#keeping only after first two underscores
colnames(minerals) <- str_extract(colnames(minerals), "[^_]*_[^_]*")

#renaming blank column to band
colnames(minerals)[1275] = "Band"

#adding wavelength as a column
minerals$wavelength = landsat_band_info$center_wavelength_nm

# Identify columns with negative values
neg_cols <- sapply(minerals, function(x) any(x < 0))

# Subset the data, excluding columns with negative values
data_subset_landsat <- minerals[, !neg_cols]

plot <- ggplot(data_subset_landsat, aes(x=wavelength)) +
  geom_line(color="blue", aes(y=data_subset_landsat$'832_chlorite'),size=1.5) +
  geom_line(color="red", aes(y=data_subset_landsat$'250_alunite'),size=1.5) +
  geom_line(color="darkgreen",aes(y=data_subset_landsat$'1817_kaolinite'),size=1.5) +
  geom_line(color="orange",aes(y=data_subset_landsat$'1080_dickite'),size=1.5) +
  geom_line(color="purple",aes(y=data_subset_landsat$'717_calcite'),size=1.5) +
  geom_point(color="white", aes(y=data_subset_landsat$'832_chlorite'),size=1.5) +
  geom_point(color="white", aes(y=data_subset_landsat$'250_alunite'),size=1.5) +
  geom_point(color="white",aes(y=data_subset_landsat$'1817_kaolinite'),size=1.5) +
  geom_point(color="white",aes(y=data_subset_landsat$'1080_dickite'),size=1.5) +
  geom_point(color="white",aes(y=data_subset_landsat$'717_calcite'),size=1.5) +
  ylim(0,1) +
  xlim(420,2500) +
  labs(y="Reflectance",
       x="Wavelength (nm)") +
  theme_half_open()


plot + theme(
  panel.background = element_rect(fill = "black"),  # Set background color
  plot.background = element_rect(fill = "black"),   # Set plot area color
  axis.text = element_text(color = "white"),        # Set axis text color
  axis.title = element_text(color = "white"),        # Set axis title color
  axis.ticks = element_line(color = "white"),
  axis.line = element_line(color = "white")
)
