library(tidyverse)
library(janitor)


landsat_8 <- read_csv("./landsat_8.csv")

landsat_8 <- landsat_8 %>% clean_names()

landsat_8 <- landsat_8 %>% mutate(Satellite_Name = "landsat_8")

saveRDS(landsat_8,"./landsat_8_band_info.rds")
