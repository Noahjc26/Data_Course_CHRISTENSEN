library(tidyverse)
library(janitor)

aster <- read_csv("./aster.csv")

aster <- aster %>% clean_names()

aster <- aster %>% mutate(Satellite_Name = "aster")

saveRDS(aster,"./aster_band_info.rds")
