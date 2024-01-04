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
aster_band_info$Wavelength_nm = (aster_band_info$lower+aster_band_info$upper)/2

#adding wavelength as a column
minerals <- minerals %>% 
  mutate(wavelength = aster_band_info$Wavelength_nm)

# Identify columns with negative values
neg_cols <- sapply(minerals, function(x) any(x < 0))

# Subset the data, excluding columns with negative values
data_subset <- minerals[, !neg_cols]

data_subset %>% 
  ggplot(aes(x=wavelength,y=data_subset$'222_alunite')) +
  geom_point() +
  ylim(0,1) +
  theme_bw()

data_subset$wavelength
class(data_subset$'222_alunite')
