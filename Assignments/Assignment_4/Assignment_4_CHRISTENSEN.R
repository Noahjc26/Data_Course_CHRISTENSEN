library(tidyverse)
list.files(path="Assignments/Assignment_4")

df <- read.csv("Assignments/Assignment_4/grngrass.csv")
#path is based on Data_Course_CHRISTENSEN as directory

ggplot(df,aes(x=wavelength,y=reflectance)) +
  geom_line()
