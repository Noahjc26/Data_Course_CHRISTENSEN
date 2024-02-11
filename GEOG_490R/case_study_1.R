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


1:length(rock_comp)

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


month_names <- precip_data$month

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
                     labels = c("Above average 30", "Below average 30"),
                     name = "Above/Below") +
  facet_wrap(~year) +
  scale_x_continuous(breaks = 1:12, labels = month_names) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 4 ####
precip_data
avg5 = NULL
std_dev5 = NULL

for (i in 1:nrow(precip_data)) {
    
  average = 
    ((precip_data$year1[i] + 
    precip_data$year2[i] + 
    precip_data$year3[i] + 
    precip_data$year4[i] +
    precip_data$year5[i])/5)
    
    std_dev = sd(c(precip_data$year1[i],
                    precip_data$year2[i],
                    precip_data$year3[i], 
                    precip_data$year4[i],
                    precip_data$year5[i]))
    
    avg5 = c(avg5, average)
    
    std_dev5 = c(std_dev5, std_dev)
}

precip_data$avg5 <- avg5
precip_data$std_dev5 <- std_dev5

precip_data %>% 
  ggplot(aes(x=month_numeric,y=std_dev5)) +
  geom_point() +
  scale_x_continuous(breaks = 1:12, labels = month_names) +
  theme_bw()
#september has the highest standard deviation

precip_data %>% 
  ggplot(aes(x=month_numeric)) +
  geom_line(aes(y=precip_data$avg5, color = "blue")) +
  geom_line(aes(y=precip_data$ave30, color = "green")) +
  scale_x_continuous(breaks = 1:12, labels = month_names) +
  scale_color_manual(values = c("blue", "green"), 
                     name = "Average Precipitation",
                     labels = c("Average over 5 years", "Average over 30 years")) +
  theme_bw()


# 30 days of energy production (kWh)
production <- c(1500, 1480, 1530, 1510, 1470, 1490, 1460, 1450, 1520, 1495,  
                1510, 1520, 1500, 1490, 1540, 1560, 1480, 1470, 1500, 1480, 
                1490, 1520, 1540, 1510, 1480, 1500, 1520, 1490, 1510, 1470)

# 30 days of solar irradiation (kW/m2) 
irradiation <- c(0.8, 0.7, 0.9, 0.85, 0.6, 0.75, 0.65, 0.7, 0.8, 0.78,   
                 0.82, 0.85, 0.78, 0.83, 0.86, 0.82, 0.8, 0.68, 0.92, 0.89,  
                 0.82, 0.85, 0.86, 0.8, 0.73, 0.81, 0.85, 0.9, 0.92, 0.88)

# 30 days of temperature (Celsius)
temp <- c(18, 17, 22, 20, 15, 16, 12, 14, 19, 21,  
          15, 17, 18, 21, 22, 23, 19, 11, 23, 25,
          18, 21, 22, 20, 19, 15, 21, 25, 26, 20)

# 30 days of degradation (per-mil)
degrade <- c(10, 10, 10, 10, 10, 12, 12, 12, 12, 12,
             10, 10, 10, 10, 10, 10, 10, 10, 10, 12, 
             12, 12, 12, 12, 12, 12, 12, 12, 12, 12)

solar_df <- data.frame(production,
                       irradiation,
                       temp,
                       degrade)

solar_df$degrade_percent = solar_df$degrade/10

solar_df$degrade_cumulative <- NA

solar_df$degrade_cumulative[1] = solar_df$degrade_percent[1]

for (i in 2:nrow(solar_df)) {
  solar_df$degrade_cumulative[i] = solar_df$degrade_cumulative[i-1] + solar_df$degrade_percent[i]
}


model <- lm(solar_df$production~solar_df$degrade_cumulative)

summary(model)

solar_df %>% 
  ggplot(aes(x=production,y=degrade_cumulative)) +
  geom_point() +
  geom_smooth()


head(solar_df)
summary(solar_df)


solar_df %>% 
  ggplot(aes(x=production,y=irradiation)) +
  geom_smooth(method = "lm") +
  geom_point(color="red") +
  theme_bw()

#it seems to be a linear plot, albeit with a large SD

production_model <- lm(solar_df$production ~ solar_df$temp + solar_df$irradiation)

summary(production_model)

# 5*1.141 increase in production with an increase in 5 degrees temperature


solar_df %>% 
  ggplot(aes(x=production,y=degrade)) +
  geom_point()


sum(solar_df$degrade_percent)
#33.2 percent degradations


#total production in GJ
sum(solar_df$production) * 0.0036



# Environmental Science
river_df <- read.csv("./cs01_river_data.csv")

#making date column in date format
river_df$date <- as.Date(river_df$date)

#making sure the dates are in order
river_df <- river_df[order(river_df$date), ]

nitrate_site_1 <- river_df[river_df$nitrate > 5 & river_df$site_id == "site1",]
nitrate_site_2 <- river_df[river_df$nitrate > 5 & river_df$site_id == "site2",]
nitrate_site_3 <- river_df[river_df$nitrate > 5 & river_df$site_id == "site3",]
nitrate_site_4 <- river_df[river_df$nitrate > 5 & river_df$site_id == "site4",]

#checking how many weeks were above nitrate standard for each site
nrow(nitrate_site_1)
nrow(nitrate_site_2)
nrow(nitrate_site_3)
nrow(nitrate_site_4)

phosphate_site_1 <- river_df[river_df$phosphate > 2 & river_df$site_id == "site1",]
phosphate_site_2 <- river_df[river_df$phosphate > 2 & river_df$site_id == "site2",]
phosphate_site_3 <- river_df[river_df$phosphate > 2 & river_df$site_id == "site3",]
phosphate_site_4 <- river_df[river_df$phosphate > 2 & river_df$site_id == "site4",]

#checking how many weeks were above phosphate standard for each site
nrow(phosphate_site_1)
nrow(phosphate_site_2)
nrow(phosphate_site_3)
nrow(phosphate_site_4)

both_site_1 <- river_df[river_df$phosphate > 2 & river_df$nitrate > 5 & river_df$site_id == "site1", ]
both_site_2 <- river_df[river_df$phosphate > 2 & river_df$nitrate > 5 & river_df$site_id == "site2", ]
both_site_3 <- river_df[river_df$phosphate > 2 & river_df$nitrate > 5 & river_df$site_id == "site3", ]
both_site_4 <- river_df[river_df$phosphate > 2 & river_df$nitrate > 5 & river_df$site_id == "site4", ]

#checking how many weeks were above phosphate and nitrate standard for each site
nrow(both_site_1)
nrow(both_site_2)
nrow(both_site_3)
nrow(both_site_4)

#finding times where it occured for 3 consecutive weeks for nitrate/site1



for (i in 1:(nrow(nitrate_site_1) - 2)) {
    
    start_date <- nitrate_site_1$date[i]
    end_date <- nitrate_site_1$date[i + 2]
    
    if (end_date - start_date == 14) {
      print(paste0("Three consecutive weeks of nitrate above standard found at site1:"))
      print(paste("Start Date:", start_date))
      print(paste("End Date:", end_date))
      print("")
    }
}

for (i in 1:(nrow(nitrate_site_2) - 2)) {
  
  start_date <- nitrate_site_2$date[i]
  end_date <- nitrate_site_2$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of nitrate above standard found at site2:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}

for (i in 1:(nrow(nitrate_site_3) - 2)) {
  
  start_date <- nitrate_site_3$date[i]
  end_date <- nitrate_site_3$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of nitrate above standard found at site3:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}

for (i in 1:(nrow(nitrate_site_4) - 2)) {
  
  start_date <- nitrate_site_4$date[i]
  end_date <- nitrate_site_4$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of nitrate above standard found at site 4:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}

#now with phosphate
for (i in 1:(nrow(phosphate_site_1) - 2)) {
  
  start_date <- phosphate_site_1$date[i]
  end_date <- phosphate_site_1$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of phosphate above standard found at site 1:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}

for (i in 1:(nrow(phosphate_site_2) - 2)) {
  
  start_date <- phosphate_site_2$date[i]
  end_date <- phosphate_site_2$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of phosphate above standard found at site 2:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}

for (i in 1:(nrow(phosphate_site_3) - 2)) {
  
  start_date <- phosphate_site_3$date[i]
  end_date <- phosphate_site_3$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of phosphate above standard found at site 3:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}

for (i in 1:(nrow(phosphate_site_4) - 2)) {
  
  start_date <- phosphate_site_4$date[i]
  end_date <- phosphate_site_4$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of phosphate above standard found at site 4:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}


#now for both sites
for (i in 1:(nrow(both_site_1) - 2)) {
  
  start_date <- both_site_1$date[i]
  end_date <- both_site_1$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of phosphate and nitrate above standard found at site 1:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}

for (i in 1:(nrow(both_site_2) - 2)) {
  
  start_date <- both_site_2$date[i]
  end_date <- both_site_2$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of phosphate and nitrate above standard found at site 2:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}

for (i in 1:(nrow(both_site_3) - 2)) {
  
  start_date <- both_site_3$date[i]
  end_date <- both_site_3$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of phosphate and nitrate above standard found at site 3:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}

for (i in 1:(nrow(both_site_4) - 2)) {
  
  start_date <- both_site_4$date[i]
  end_date <- both_site_4$date[i + 2]
  
  if (end_date - start_date == 14) {
    print(paste0("Three consecutive weeks of phosphate and nitrate above standard found at site 4:"))
    print(paste("Start Date:", start_date))
    print(paste("End Date:", end_date))
    print("")
  }
}


#8 
river_df %>% 
  ggplot(aes(x=date,y=nitrate,color = site_id)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = nitrate,fill = site_id), alpha = 0.5) +
  labs(title = "Nitrate Levels Over Time",
       x = "Date",
       y = "Nitrate") +
  facet_wrap(~site_id) +
  geom_line(y=5,linewidth = 1,color = "black")
  theme_bw()

river_df %>% 
  ggplot(aes(x=date,y=phosphate,color = site_id)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = phosphate,fill = site_id), alpha = 0.5) +
  labs(title = "Phosphate Levels Over Time",
       x = "Date",
       y = "Phosphate") +
  facet_wrap(~site_id) +
  geom_line(y=2,linewidth = 1,color = "black")
  theme_bw()
