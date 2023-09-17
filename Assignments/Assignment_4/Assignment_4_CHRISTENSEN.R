library(tidyverse)

df <- read.csv("grngrass.csv")


ggplot(df,aes(x=wavelength,y=reflectance)) +
  geom_line()
