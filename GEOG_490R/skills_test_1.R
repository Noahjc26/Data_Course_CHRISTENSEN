library(tidyverse)

data <- read.csv("https://raw.githubusercontent.com/mattols/geospat_data/main/earth-jobs.csv")
data

#1
# List age and gender of the highest and lowest paid employees
data[which.max(data$salary),c(2:3,7)]
data[which.min(data$salary),c(2:3,7)]


#2 
#ordering dataset by salary in descending order
data_ordered <- data[order(data$salary),]

#3
#List the employees with a salary greater than $80000 whose hire data is before 2020-01-01
data_ordered$hire_date <- as.Date(data_ordered$hire_date)

data_ordered[data_ordered$salary > 80000 & data_ordered$hire_date < "2020-01-01",]


#4
med_geo <- median(data_ordered[data_ordered$area == "Geology",]$salary)

med_envt <- median(data_ordered[data_ordered$area != "Geology",]$salary,na.rm = T)

#difference between median
med_geo-med_envt

#5
data_ordered$date_diff <- Sys.Date() - data_ordered$hire_date

data_ordered$year_diff <- as.numeric(data_ordered$date_diff / 365.25)


#6
data_ordered %>% 
  ggplot(aes(x=age,y=salary,col=gender)) +
  geom_point()+
  facet_wrap(~area) +
  theme_bw()

#granted this is a smaller dataset but it almost seems like there is a negative coorelation between age and salary, espeically for environmental science
#While it does seem like being male you would be making more based on this graph, there is simply fewer female datapoints (which could be its own problem)

data_ordered %>% 
  ggplot(aes(x=education,y=salary,fill=gender)) +
  geom_col(position = "dodge") +
  facet_wrap(~job) +
  theme_bw()

#To get the hgihest pay by education you should aim at getting a Masters, this makes signiicantly more than any of the other areas, including a PhD
#While there are less female datapoints it does seem that men with the same education typically make a couple grand more a year. Except for data science jobs.

#7

summary(lm(salary ~education + job + area + gender + age,data_ordered))
# It seems like the majority of these variables can predict salary, given them all together you could get a pretty good guess givben a p-value of 4.8E-7 and an R^2 of 0.9115

##2
#8

vect <- sample(100)

#Creating sample variance function
sam_var <- function(vect) {
  #creating total variable
  total = 0
  
  #calcuating the total sum of the vector
  for (i in 1:length(vect)) {
    total <- total + vect[i]
  }

  #calculating the mean
  v_mean <- total / length(vect)
  
  #creating total square distance variable
  tot_sq_dist = 0
  
  #loop to calculate total square distance
  for (i in 1:length(vect)) {
    sq_dist <- (vect[i]-v_mean)^2
    tot_sq_dist = tot_sq_dist + sq_dist
  }
  sample_variance <- tot_sq_dist/(length(vect)-1)
  
  return(sample_variance)
}

sam_var(vect)
