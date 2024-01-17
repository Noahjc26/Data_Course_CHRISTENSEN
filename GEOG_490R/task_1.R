library(tidyverse)

# Creating dataframe with depths and magnitude
df <- data.frame(depth = c(5,15,8,60,10,9,33,20,52,38,49,31),
                 magnitude = c(0.1,10,5.5,2,6,4.4,7.2,5.5,2,1.1,8.8,3.3))

# Using which.max function to see which index has the highest
max_magnitude_index <- which.max(df$magnitude)

# Printing row with highest magnitude to check depth
(df[max_magnitude_index,])

# Checking mean for magnitude and depth
mean(df$magnitude)
mean(df$depth)

# These statistics are describing the average magnitude and average depth for all 12 values in each vector

# Checking median for magnitude and depth
median(df$magnitude)
median(df$depth)

# These statistics are describing the middle value between the magnitude and depth. It is calculated from ordering the values lowest to highest and taking the average between the 6th and 7th value because there are 12 values.

ggplot(df,aes(x=magnitude,y=depth)) +
  geom_point() +
  theme_bw()

# This does not seem like there is any correlation to depth and magnitude


# Working on part 2

# Sample data for pm25 
site1_pm25 <- c(10, 15, 22, 18, 12, 23, 16, 20, 18, 14, 15, 13, 12, 10) 
site2_pm25 <- c(11, 17, 28, 21, 18, 23, 15, 22, 20, 15, 17, 17, 13, 12) 

# Sample data for ozone
site1_ozone <- c(65, 82, 71, 55, 63, 88, 79, 63, 72, 81, 89, 95, 70, 67)

# Listing mean and median for ozone, PM2.5 site 1, PM2.5 site 2, and overall PM2.5
mean(site1_ozone)
median(site1_ozone)

mean(site1_pm25)
median(site1_pm25)

mean(site2_pm25)
median(site2_pm25)

mean(c(site1_pm25,site2_pm25))
median(c(site1_pm25,site2_pm25))

# Creating dataframe with new date column
date <- c(10:23)
df2 <- data.frame(date = date,
                    site1_ozone = site1_ozone,
                    site1_pm25 = site1_pm25,
                    site2_pm25 = site2_pm25,
                  overall_pm25 = (site1_pm25 + site2_pm25)/2)




# Finding index with the highest value for ozone
max_ozone_index <- which.max(df2$site1_ozone)

df2[max_ozone_index,]

# March 21st 2022 has the highest ozone

# Finding index with the lowest value for ozone
min_ozone_index <- which.min(df2$site1_ozone)

df2[min_ozone_index,]

# March 13th 2022 has the lowest ozone

# Finding index with the highest value for PM2.5
max_pm25_index <- which.max(df2$overall_pm25)

df2[max_pm25_index,]

# March 12th 2022 has the highest pm25

# Finding index with the lowest value for PM2.5
min_pm25_index <- which.min(df2$overall_pm25)

df2[min_pm25_index,]

# March 10th 2022 has the lowest pm25

# Creating plot to see change in values for ozone and pm sites over the dates
ggplot(df2,aes(x=date)) +
  geom_point(aes(y=site1_ozone),color="red") +
  geom_point(aes(y=site2_pm25),color="blue") +
  geom_point(aes(y=site1_pm25),color="green") +
  labs(y="") +
  theme_bw()
  
# PM and ozone do not seem to follow the same pattern given the day, but the PM values between sites seem to follow a very similar pattern by the day.
# It looks like the ozone values may be generally increasing while the PM values might be slightly decreasing over time.

# Finding average distance between pm sites
mean(site1_pm25) - mean(site2_pm25)

# Looking at the plot no values go over 100 for the ozone and only 1 value for site2_pm25 goes over 25 on March 12th 2022

# Calculating standard deviation for PM2.5 and ozone

sd(df2$overall_pm25)
sd(df2$site1_ozone)

# The standard deviation tells us the spread of our data, given 2 standard deviations 95% of our data would plot in that range from the mean. From our results we can see that ozone has a wider range of values from the mean, while the pm25 values all surround closer to the mean.

# Looking at percent increase in PM2.5 from the cleanest day to the most polluted day

percent_increase_site1 <- max(df2$site1_pm25) / min(df2$site1_pm25) * 100
percent_increase_site2 <- max(df2$site2_pm25) / min(df2$site2_pm25) * 100
percent_increase_overall <- max(df2$overall_pm25) / min(df2$overall_pm25) * 100

percent_increase_site1
percent_increase_site2
percent_increase_overall

