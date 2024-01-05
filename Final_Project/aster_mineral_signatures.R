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

aster_band_info <- readRDS("./aster_band_info.rds")

list <- list.files("../../usgs_spectral_library/usgs_splib07 (1)/ASCIIdata/ASCIIdata_splib07b_rsASTER/ChapterM_Minerals/",
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
colnames(minerals)[1277] = "Band"

#creating column that is wavelength
aster_band_info$wavelength = (aster_band_info$lower+aster_band_info$upper)/2

#adding wavelength as a column
minerals <- minerals %>% 
  mutate(wavelength = aster_band_info$Wavelength_nm)

# Identify columns with negative values
neg_cols <- sapply(minerals, function(x) any(x < 0))

# Subset the data, excluding columns with negative values
data_subset_aster <- minerals[, !neg_cols]

plot <- ggplot(data_subset_aster, aes(x=wavelength)) +
  geom_line(color="blue", aes(y=data_subset_aster$'805_chlorite'),size=1) +
  geom_line(color="red", aes(y=data_subset_aster$'222_alunite'),size=1) +
  geom_line(color="darkgreen",aes(y=data_subset_aster$'1789_kaolinite'),size=1) +
  geom_line(color="orange",aes(y=data_subset_aster$'1052_dickite'),size=1) +
  geom_line(color="purple",aes(y=data_subset_aster$'689_calcite'),size=1) +
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
    