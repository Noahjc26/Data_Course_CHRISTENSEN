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

#alunite spectral signature
data_subset %>% 
  ggplot(aes(x=wavelength,y=data_subset$'222_alunite')) +
  geom_line(color="red") +
  ylim(0,1) +
  labs(y="Reflectance",
         x="Wavelength") +
  theme_bw()

#chlorite spectral signature
plot <- data_subset %>% 
  ggplot(aes(x=wavelength)) +
  geom_line(color="blue",aes(y=data_subset$'805_chlorite'),size=1) +
  geom_line(color="red",aes(y=data_subset$'222_alunite'),size=1) +
  geom_line(color="forestgreen",aes(y=data_subset$'1801_kaolinite'),size=1) +
  geom_line(color="orange",aes(y=data_subset$'1031_dickite'),size=1) +
  geom_line(color="purple",aes(y=data_subset$'668_calcite'),size=1) +
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

#Propylitic alteration: chlorite, epidote, and sericite.
#Phyllic alteration: muscovite, kaolinite, and sericite.
#Argillic alteration: kaolinite, halloysite, and dickite.
#Silicic alteration: quartz, silica, and chalcedony.
#Advanced argillic alteration: pyrophyllite, diaspore, and kaolinite.
#Potassic alteration: K-feldspar and biotite.
#Sodic alteration: albite and nepheline.

# Extract numeric data from the dataframe
data_matrix <- as.matrix(t(data_subset[, -c(885,886)]))

# Create a hyperSpec object
hyperspec_object <- new("hyperSpec", wavelength = data_subset$wavelength, spc = data_matrix)

plot(hyperspec_object[325])

hy_sub <- hyperspec_object[325]

str(hy_sub)


#keeping on values below wavelength 1650
plot(hy_sub@data$spc[hy_sub@wavelength <= 1650 & hy_sub@wavelength >= 1350])

names(data_subset)[325]

& hy_sub@wavelength >= 1350]
length(hy_sub@data$spc)
hy_sub@wavelength <= 1650
#-------------------------------










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

