#Case Study 2
library(tidyverse)
library(terra)

##### 1 #####
#1.
avy <- read.csv("https://raw.githubusercontent.com/mattols/geospat_data/main/avalanches_2012_2024.csv")  

#turning date column into date format
avy$Date <- as.Date(avy$Date)

#making new year column based on the year from the date column
avy <- mutate(avy, year = format(Date, "%Y"))

#making new month column based on the month from the date column
avy <- mutate(avy, month = format(Date, "%m"))

# Total number of avafactor(avy$year)

# Plotting
ggplot(avy, aes(x = as.factor(year))) +
  geom_bar()
# Total number of avalanches by month
ggplot(avy, aes(x=factor(month))) +
  geom_bar()

# Total number of avalanches in each region
ggplot(avy, aes(x = factor(Region))) +
  geom_bar()

# All avalanches by date for the 2020-2021 season
avy %>% 
  filter(Date > "2020-10-01" & Date < "2021-10-01") %>% 
  ggplot( aes(x= Date)) +
  geom_bar() +
  theme_bw()

# Avalanches seem to happen most often directly in the middle of the water season from 2020-2021 ,  they seem to be increasing in amount over the last 12 years, January has the most occurences, and salt lake city has by far the most occurences

# Advanced section
buried <- avy %>% 
  filter(!is.na(Buried...Fully) | !is.na(Buried...Partly)) %>% 
  nrow()

buried / nrow(avy) * 100
# 2.52% chance to be buried in an avalanche in Utah

buried <- avy %>% 
  filter(!is.na(Buried...Fully) | !is.na(Buried...Partly)) %>% 
  nrow()

