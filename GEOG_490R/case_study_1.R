library(tidyverse)
library(Ternary)

#reading in data matrix
rock_comp <- matrix(c(43.18, 22.49, 34.33,  
                      8.19, 34.61, 57.20,
                      27.28, 55.12, 17.60,
                      62.95, 16.55, 20.50, 
                      47.83, 11.77, 40.39,
                      58.92, 6.50, 34.58,
                      18.42, 21.35, 60.23,
                      18.38, 15.16, 66.47,
                      19.41, 42.25, 38.34,
                      65.16, 1.91, 32.93, 
                      29.43, 24.08, 46.49,
                      25.32, 36.72, 37.96,
                      11.66, 46.38, 41.96,
                      40.35, 23.41, 36.24,
                      52.34, 38.90, 8.76,
                      2.54, 67.07, 30.39,
                      55.56, 39.70, 4.74,
                      9.32, 20.84, 69.84,
                      39.24, 27.34, 33.42,
                      25.82, 40.90, 33.28,
                      31.66, 36.99, 31.34,
                      47.52, 27.74, 24.74,
                      33.00, 60.59, 6.41, 
                      45.33, 7.94, 46.73,
                      14.68, 35.34, 49.99),
                    nrow=25, ncol=3, byrow = TRUE)

TernaryPlot(alab = expression(bold('Q')), 
            blab = expression(bold('A')),
            clab = expression(bold('P')))
            
            TernaryPoints(rock_comp, col='blue', pch=15, cex=0.5)
            
#turning into dataframe
rock_comp <- as.data.frame(rock_comp)

#setting column names
colnames(rock_comp) <- c("Q","A","P")

#creating new column that will be the class
rock_comp$class = NA

# Apply conditions
for (i in 1:nrow(rock_comp)) {
  if (rock_comp$Q[i] > 60) {
    rock_comp$class[i] <- NA
  } else if (rock_comp$P[i] >= 65 & rock_comp$Q[i] < 20) {
    rock_comp$class[i] <- "andesite/basalt"
  } else if (rock_comp$P[i] >= 65 & rock_comp$Q[i] > 20) {
    rock_comp$class[i] <- "dacite"
  } else if (rock_comp$P[i] < 65 & rock_comp$Q[i] < 20 & rock_comp$P[i] > 35){
    rock_comp$class[i] <- "latite"
  } else if (rock_comp$P[i] < 35 & rock_comp$Q[i] < 20){
    rock_comp$class[i] <- "trachyte"
  } else if (rock_comp$Q[i] > 20 & rock_comp$P[i] < 65){
    rock_comp$class[i] <- "rhyolite"
  } else {
    rock_comp$class[i] <- "not classified"
  }
}

rock_comp %>% 
  ggplot(aes(x=class,fill=class)) +
  geom_bar() +
  labs(x="Rock Class",
       y= "Number of samples",
       main = "") +
  theme_bw()

#I think this solution is efficient and correct if you are looking for those six classifications, but if you are needing to split it into any further classifications there might be a better method for that.


precip_data <- data.frame(
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  year1 = c(3.879, 10.430, 6.991, 10.071, 17.864, 21.627, 20.853, 16.377, 10.000, 2.754, 3.507, 5.033),
  year2 = c(4.540, 7.922, 8.719, 12.649, 19.564, 26.676, 19.410, 16.108, 12.000, 4.194, 2.943, -1.098),
  year3 = c(8.117, 4.470, 8.637, 11.067, 17.948, 25.307, 21.790, 14.876, 13.000, 4.067, 2.914, 3.169),
  year4 = c(5.141, 5.626, 9.217, 12.242, 18.542, 22.724, 21.756, 14.388, 15.000, 6.560, 5.737, 2.248),
  year5 = c(5.259, 6.109, 8.888, 14.054, 18.750, 27.508, 21.643, 14.239, 18.000, 4.833, 2.548, 2.432),
  ave30 = c(4.491, 6.015, 9.719, 14.322, 17.637, 23.872, 20.194, 14.301, 9.587, 3.585, 2.633, 1.460)
)

precip_data$month_numeric = c(1:12)

#creating new vector with months in each year above 30 year average and below 30 year average

for (i in 1:nrow(precip_data)) {
  if (precip_data$year1[i] > precip_data$ave30[i]) {
    precip_data$above1[i] = "blue"
  } else {
    precip_data$above1[i] = "red"
  }
  if (precip_data$year2[i] > precip_data$ave30[i]) {
    precip_data$above2[i] = "blue"
  } else {
    precip_data$above2[i] = "red"
  }
  if (precip_data$year3[i] > precip_data$ave30[i]) {
    precip_data$above3[i] = "blue"
  } else {
    precip_data$above3[i] = "red"
  }
  if (precip_data$year4[i] > precip_data$ave30[i]) {
    precip_data$above4[i] = "blue"
  } else {
    precip_data$above4[i] = "red"
  }
  if (precip_data$year5[i] > precip_data$ave30[i]) {
    precip_data$above5[i] = "blue"
  } else {
    precip_data$above5[i] = "red"
  }
}


long_precip <- pivot_longer(precip_data, cols = c(starts_with("year"),"ave30"),
                            names_to = "year",
                            values_to = "precip")

long_precip <- pivot_longer(long_precip, cols = starts_with("above"),
                            names_to = "above",
                            values_to = "color")

for (i in 1:nrow(long_precip)) {
  if (long_precip$year[i] == "year1" & long_precip$above[i] != "above1") {
    long_precip$color[i] = NA
  }
  if (long_precip$year[i] == "year2" & long_precip$above[i] != "above2") {
    long_precip$color[i] = NA
  }
  if (long_precip$year[i] == "year3" & long_precip$above[i] != "above3") {
    long_precip$color[i] = NA
  }
  if (long_precip$year[i] == "year4" & long_precip$above[i] != "above4") {
    long_precip$color[i] = NA
  }
  if (long_precip$year[i] == "year5" & long_precip$above[i] != "above5") {
    long_precip$color[i] = NA
  }
  if (long_precip$year[i] == "ave30") {
    long_precip$color[i] = "NULL"
  }
}


#remove rows with NA values in the "color" column
long_precip <- subset(long_precip, !is.na(color))


ggplot(long_precip, aes(x = month_numeric, y = precip)) +
  geom_line() +
  geom_point(aes(color = color)) +
  scale_color_manual(values = c("red" = "red", "blue" = "blue"),
                     labels = c("Above average 30", "Below average 30")) +
  facet_wrap(~year) +
  scale_x_continuous(breaks = 1:12, labels = month_names) +
  theme_bw() +
  labs(color = "Above/Below") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 4 ####


