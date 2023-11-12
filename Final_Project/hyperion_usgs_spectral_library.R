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

#reading in hyperion data
hyperion_band_info <- readRDS("./hyperion_band_info.rds")

#reading in spectral mineral signatures
list <-
  list.files(path = "../../usgs_spectral_library/usgs_splib07 (1)/ASCIIdata/ASCIIdata_splib07b_cvHYPERION/ChapterM_Minerals/",
             full.names = TRUE)

#list applying as csv and into a data frame
minerals <- list %>%
  lapply(read.csv) %>%
  as.data.frame() %>%
  clean_names() %>%
  mutate(Bands = row_number())

#removing non-working bands
minerals <- minerals[c(8:57, 77:224), ]

#removing everything before first underscore
colnames(minerals) = sub(".*?_", "", colnames(minerals))

#removing everything before first underscore
colnames(minerals) = sub(".*?_", "", colnames(minerals))

#keeping only after first two underscores
colnames(minerals) <- str_extract(colnames(minerals), "[^_]*_[^_]*")

#renaming blank column to band
colnames(minerals)[1277] = "Band"

#adding wavelength as a column
minerals <- minerals %>% 
  mutate(wavelength = hyperion_band_info$Wavelength_nm)

#------------------------------------
# Identify columns with negative values
neg_cols <- sapply(minerals, function(x) any(x < 0))

# Subset the data, excluding columns with negative values
data_subset <- minerals[, !neg_cols]

# Extract numeric data from the dataframe
data_matrix <- as.matrix(t(data_subset[, -c(885,886)]))

# Create a hyperSpec object
hyperspec_object <- new("hyperSpec", wavelength = data_subset$wavelength, spc = data_matrix)


plot(hyperspec_object)



#--------------------------------










#turning into matrix and back to dataframe so col and row are switched
minerals <- data.frame(t(minerals[-1227]))

#--------
#adding row name (minerals) into a column of their own
minerals <- minerals %>%
  mutate(mineral = rownames(minerals))

#renumbering rows instead of mineral names
rownames(minerals) <- c(1:1277)

#delete row 1276
minerals <- minerals[-1276, ]
#----------

#getting rid of negative rows
minerals <- minerals[minerals$X8 >= 0,]
minerals <- minerals[minerals$X192 >= 0,]


# This function almost renames everything except B009 and B009 are set as B8 and B9
rename_columns_1 <- function(df) {
  new_names <- colnames(df)
  new_names <-
    sub("^X([0-9])$", "B00\\1", new_names)  # Match and replace 'X8' and 'X9'
  new_names <- sub("^X", "B0", new_names)  # Replace 'X' with 'B0'
  new_names <- sub("^B0+", "B", new_names)  # Remove leading zeros
  new_names <-
    sub("B$", "B00", new_names)  # Add two trailing zeros if it ends with 'B'
  new_names <-
    sub("B(\\d{2}$)", "B0\\1", new_names)  # Add one leading zero
  colnames(df) <- new_names
  return(df)
}

# Call the function and pass your dataframe as an argument
minerals <- rename_columns_1(minerals)


# This changes B8 and B9 into B008 and B009
rename_columns_2 <- function(df) {
  new_names <- colnames(df)
  new_names <-
    sub("^B([0-9])$", "B00\\1", new_names)  # Match and replace 'X8' and 'X9'
  colnames(df) <- new_names
  return(df)
}

# Call the function and pass your dataframe as an argument
minerals <- rename_columns_2(minerals)

# Remove everything before and including the underscore
minerals$mineral <- sub(".*_", "", minerals$mineral)

#setting as a factor
minerals$mineral <- as.factor(minerals$mineral)

#thats a lot of minerals
unique(minerals$mineral)

#saving as RDS
saveRDS(object = minerals, file = "./cleaned_mineral_signatures.rds")

