library(tidyverse)

#reading in dataframes
state_elev <- read.csv("./task_4/messy_state_elev.csv")
us_pop <- read.csv("./task_4/us-pop.csv",skip = 4)
us_temp <- read.csv("./task_4/us-temp-dec-june.csv")

## #1

#removing rows with NA
us_temp <- us_temp[1:48,]

us_temp$diff_june_22 <- us_temp$june_2022 - us_temp$june_means
us_temp$diff_june_23 <- us_temp$june_2023 - us_temp$june_means

us_temp$diff_dec_22 <- us_temp$december_2022 - us_temp$december_means
us_temp$diff_dec_23 <- us_temp$december_2023 - us_temp$december_means

us_temp <- us_temp[order(us_temp$diff_dec_22), ]

#looking at the lowest temperatures below average
us_temp %>% 
  select(name,diff_dec_22) %>% 
  head(5)

#looking at the highest temperatures above the average
us_temp %>% 
  select(name,diff_dec_22) %>% 
  tail(5)

#now lets order by 2023 and get the top and lowest 5
us_temp <- us_temp[order(us_temp$diff_dec_23), ]

#looking at the lowest temperatures below average
us_temp %>% 
  select(name,diff_dec_23) %>% 
  head(5)

#looking at the highest temperatures above the average
us_temp %>% 
  select(name,diff_dec_23) %>% 
  tail(5)

#state in each region with highest june anomaly
us_temp %>%
  filter(region == "Midwest") %>%
  arrange(desc(diff_june_22)) %>%
  slice(c(1, n())) %>%
  select(name, diff_june_22)


us_temp %>%
  filter(region == "South") %>%
  arrange(desc(diff_june_22)) %>%
  slice(c(1, n())) %>%
  select(name, diff_june_22)

us_temp %>%
  filter(region == "West") %>%
  arrange(desc(diff_june_22)) %>%
  slice(c(1, n())) %>%
  select(name, diff_june_22)

us_temp %>%
  filter(region == "Northeast") %>%
  arrange(desc(diff_june_22)) %>%
  slice(c(1, n())) %>%
  select(name, diff_june_22)

#now lets do it for 2023
us_temp %>%
  filter(region == "Midwest") %>%
  arrange(desc(diff_june_23)) %>%
  slice(c(1, n())) %>%
  select(name, diff_june_23)

us_temp %>%
  filter(region == "South") %>%
  arrange(desc(diff_june_23)) %>%
  slice(c(1, n())) %>%
  select(name, diff_june_23)

us_temp %>%
  filter(region == "West") %>%
  arrange(desc(diff_june_23)) %>%
  head(1) %>%
  select(name, diff_june_23)

us_temp %>%
  filter(region == "Northeast") %>%
  arrange(desc(diff_june_23)) %>%
  slice(c(1, n())) %>%
  select(name, diff_june_23)


## #2
us_temp_long <- pivot_longer(us_temp, cols = starts_with("diff"),
                             names_to = "month_year",
                             values_to = "difference")

us_temp_long %>%
  ggplot(aes(x=difference,fill = month_year)) +
  geom_histogram() +
  facet_wrap(~month_year) +
  theme_bw()


# next tuesday ----

# pivot tables
# facet wrap



## #3

us_temp
for (i in 1:nrow(us_temp)) {
  if (us_temp$diff_june_22[i] > 0 & us_temp$diff_june_23[i] > 0 ) {
    us_temp$june_category[i] = "increasing"
  } else if(us_temp$diff_june_22[i] < 0 & us_temp$diff_june_23[i] < 0){
    us_temp$june_category[i] = "decreasing"
  } else if((us_temp$diff_june_22[i] < 0 & us_temp$diff_june_23[i] > 0) | (us_temp$diff_june_22[i] > 0 & us_temp$diff_june_23[i] < 0)){
    us_temp$june_category[i] = "mixed"
  }
}

for (i in 1:nrow(us_temp)) {
  if (us_temp$diff_dec_22[i] > 0 & us_temp$diff_dec_23[i] > 0 ) {
    us_temp$dec_category[i] = "increasing"
  } else if(us_temp$diff_dec_22[i] < 0 & us_temp$diff_dec_23[i] < 0){
    us_temp$dec_category[i] = "decreasing"
  } else if((us_temp$diff_dec_22[i] < 0 & us_temp$diff_dec_23[i] > 0) | (us_temp$diff_dec_22[i] > 0 & us_temp$diff_dec_23[i] < 0)){
    us_temp$dec_category[i] = "mixed"
  }
}

us_temp %>% 
  ggplot(aes(x=june_category,fill=june_category)) +
  geom_bar()

us_temp %>% 
  ggplot(aes(x=dec_category,fill=dec_category)) +
  geom_bar()

## #4

us_temp_long %>% 
  ggplot(aes(x=june_means,y=difference,color=region)) +
  geom_point() +
  facet_wrap(~month_year) +
  theme_bw()

## #5


## #2
## #6

#changing column names
names(state_elev) <- c("state","max_elev_location","max_elev_county","max_elev_ft","min_elev_location","min_elev_county","min_elev_ft")

#remove special characters
#removing anything between the brackets AND the brackets
state_elev$min_elev_ft <- gsub("\\{.*\\}","",state_elev$min_elev_ft)

#replace Sea level with 0
state_elev$min_elev_ft <- gsub("Sea level",0,state_elev$min_elev_ft)

#replace Sea Level with 0
state_elev$min_elev_ft <- gsub("Sea Level",0,state_elev$min_elev_ft)

#removing commas
state_elev$min_elev_ft <- gsub(",","",state_elev$min_elev_ft)

#changing numeric columns to the correct class
state_elev$min_elev_ft <- as.numeric(state_elev$min_elev_ft)


#lets repeat these steps for the other elevation column
#removing anything between the brackets AND the brackets
state_elev$max_elev_ft <- gsub("\\{.*\\}","",state_elev$max_elev_ft)

#removing commas
state_elev$max_elev_ft<- gsub(",","",state_elev$max_elev_ft)

#changing numeric columns to the correct class
state_elev$max_elev_ft <- as.numeric(state_elev$max_elev_ft)

#changing - - to NA
state_elev[state_elev == "- -"] <- NA

#making elevation range column
state_elev$elev_range = state_elev$max_elev_ft - state_elev$min_elev_ft

summary(state_elev)
head(state_elev)

## #3
## #7
#renaming X to state
us_pop <- us_pop %>% 
  rename(state = X)
  
#removing unnecessary bottom rows
us_pop <- us_pop[1:57,]

#removing commas through the entire dataframe
us_pop <- mutate_all(us_pop, ~gsub(",", "", .))

#turning all columns except the state into numeric
us_pop[, 2:6] <- lapply(us_pop[, 2:6], as.numeric)
us_pop

#calculating percent change between each decade
us_pop$percent_change_2020 = ((us_pop$Resident.Population.2020.Census /us_pop$Resident.Population.2010.Census)-1) * 100
us_pop$percent_change_2010 = ((us_pop$Resident.Population.2010.Census /us_pop$Resident.Population.2000.Census)-1) * 100
us_pop$percent_change_2000 = ((us_pop$Resident.Population.2000.Census /us_pop$Resident.Population.1990.Census)-1) * 100
us_pop$percent_change_1990 = ((us_pop$Resident.Population.1990.Census /us_pop$Resident.Population.1980.Census)-1) * 100

#looking at 10 max percentage changes and the states
us_pop[order(us_pop$percent_change_2020, decreasing = TRUE), ][1:10,c(1,7)]

## #8
pop_elev <- full_join(us_pop,state_elev)

pop_elev %>%
  ggplot(aes(x = percent_change_2020, y = elev_range)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add linear model line
  theme_bw() +
  labs(x="Percent change from 2010 to 2020",
       y="Elevation range from lowest to highest point")

#creating the linear model to look at the numbers
model <- lm(percent_change_2020 ~ elev_range, data = pop_elev)
summary(model)

## #9
states_to_exclude <- c("Alaska", "California", "Delaware", "District of Columbia", "Florida", "Hawaii", "New Mexico", "North Dakota", "Puerto Rico", "West Virginia", "Wyoming")

filtered_pop_elev <- pop_elev %>%
  filter(!state %in% states_to_exclude)


filtered_pop_elev %>%
  ggplot(aes(x = percent_change_2020, y = elev_range)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add linear model line
  theme_bw() +
  labs(x="Percent change from 2010 to 2020",
       y="Elevation range from lowest to highest point")

#creating the linear model to look at the numbers
model <- lm(percent_change_2020 ~ elev_range, data = filtered_pop_elev)
summary(model)

## #10

long_pop_elev <- pop_elev %>% pivot_longer(starts_with("percent"),
                          names_to = "year",
                          values_to = "percent_change")

long_pop_elev %>% 
  ggplot(aes(x=year,y=percent_change,fill=year)) +
  geom_boxplot()

#im not exactly sure what question 10 is asking

## #11
us_temp <- rename(us_temp,state = name)

pop_elev_temp <- full_join(pop_elev,us_temp)

pop_elev_temp %>% 
  ggplot(aes(x=percent_change_2020,y=max_elev_ft)) +
  geom_point()

pop_elev_temp %>% 
  ggplot(aes(x=june_means,y=max_elev_ft)) +
  geom_point()

  