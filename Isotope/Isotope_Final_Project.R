library(tidyverse)
library(janitor)
library(leaflet)
library(sp)
library(sf)


coor <- read.csv("./Coordinates.csv",na.strings=c("","NA")) %>% 
  clean_names()

coor <- coor %>% 
  rename(trap_number = trap_coordinates) %>% 
  separate(trap_number, into = c("temp","trap_number")) %>% 
  select(-temp)

bug_data <- read.csv("./Bug_Data.csv",na.strings=c("","NA")) %>% 
  clean_names()

bug_data <- bug_data %>% 
  separate(sample_id, into = c("temp","trap_number","temp2")) %>% 
  select(-c("temp","temp2"))

df <- full_join(bug_data,coor)

df <- df[!is.na(df$latitude),]

write_csv(df,"./cleaned_bug_data.csv",)




#trying to map in r, but not really working :(


coordinates(df) <- ~longitude+latitude

#convert to sf object
map <- st_as_sf(df)

#set crs (WSG1984 seems to be used here)
st_crs(map) <- 4326

#create leaflet
leaflet(map) %>% addMarkers(label = df$well_number) %>% addTiles()

leaflet(map) %>% addTiles() %>%
  addLabelOnlyMarkers(label =  ~as.character(trap_number), 
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "15px",offset = c(-4,-20))) %>% 
  addMarkers()
?leaflet::labelOptions()

