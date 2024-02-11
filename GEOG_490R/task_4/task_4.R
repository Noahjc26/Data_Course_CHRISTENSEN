library(tidyverse)

#reading in dataframes
state_elev <- read.csv("./task_4/messy_state_elev.csv")
us_pop <- read.csv("./task_4/us-pop.csv")
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
