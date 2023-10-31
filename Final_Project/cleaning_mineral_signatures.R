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

#reading in spectral mineral signatures
minerals <- list.files(path="../../usgs_spectral_library/usgs_splib07 (1)/ASCIIdata/ASCIIdata_splib07b_cvHYPERION/ChapterM_Minerals/",
                       full.names=TRUE)

#list applying as csv and into a data frame
minerals <- minerals %>% 
  lapply(read.csv) %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  mutate(Bands = row_number())

#removing non-working bands
minerals <- minerals[c(8:57,77:224),]

#removing everything before first underscore
colnames(minerals) = sub(".*?_", "", colnames(minerals))

#removing everything before first underscore
colnames(minerals) = sub(".*?_", "", colnames(minerals))

#keeping only after first two underscores
colnames(minerals) <- str_extract(colnames(minerals), "[^_]*_[^_]*")

#renaming blank column to band
colnames(minerals)[1277] = "Band"

#turning into matrix and back to dataframe so col and row are switched
df2 <- data.frame(t(minerals[-1227]))

#adding row name (minerals) into a column of their own
df3 <- df2 %>% 
  mutate(mineral = rownames(df2))

#renumbering rows instead of mineral names
rownames(df3)<-c(1:1276)

#delete row 1276
min_sig <- df3[-1276,]


#getting rid of negative rows
min_sig <- min_sig[min_sig$X8 >= 0, ]
min_sig <- min_sig[min_sig$X192 >= 0, ]



long_min <- min_sig %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "bands",
               values_to = "reflectance")



long_min %>% 
  ggplot(aes(x=bands,y=reflectance)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,size = 4))


#saving as RDS
saveRDS(object = df3,file = "./cleaned_mineral_signatures.rds")
