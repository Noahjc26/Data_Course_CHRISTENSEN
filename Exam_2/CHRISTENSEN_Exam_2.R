library(tidyverse)
library(janitor)
library(modelr)
library(easystats)

df <- read.csv("./unicef-u5mr.csv") %>% 
  clean_names()

cdf <- df %>% 
pivot_longer(cols = starts_with("u5mr"),
             values_to = "deaths_per_1000",
             names_to = "year") %>% 
  separate(col = year,into = c("temp","year")) %>% 
  select(-temp)

cdf$year = as.numeric(cdf$year)

p <- cdf %>% 
  ggplot(aes(x=year,y=deaths_per_1000,fill=country_name)) +
  geom_line() +
  facet_wrap(~continent) +
  theme_bw() +
  labs(x="Year",
       y="U5MR")

p
ggsave("./CHRISTENSEN_plot_1.png",p)


#finding mean for deaths by groups of continent and year
grouped <- aggregate(cdf$deaths_per_1000, list(cdf$continent,cdf$year), FUN=mean, na.rm = TRUE) 
#renaming columns
colnames(grouped) <- c("continent","year","mean_U5MR")

p2 <- grouped %>% 
  ggplot(aes(x=year,y=mean_U5MR,color=continent)) +
  geom_line(size=2) +
  theme_bw()

p2
ggsave("./CHRISTENSEN_plot_2.png",p2)

#creating models

#mod1 is looking at total average of deaths according to year
mod1 <- glm(deaths_per_1000 ~ year, data = cdf)
mod1$aic

#mod2 is looking at total average deaths but separating into intercepts by continent
mod2 <- glm(deaths_per_1000 ~ continent + year, data = cdf)
mod2$aic

#mod3 is looking at total deaths per continent NOT bound by the average regression line
mod3 <- glm(deaths_per_1000 ~ continent * year, data = cdf)
mod3$aic

#if you are asked overall global deaths over time then model 1 would be the best for predictions, if you were asked by continent then I would say that
#model 3 is the best since it does not force all of the regression lines into a specific slope like model 2 does
#model 3 allows for a change in slope and intercept with allows more accuracy when predicting data based on continent

grid <- cdf %>% 
  data_grid(continent, year) %>% 
  gather_predictions(mod1, mod2, mod3)

p3 <- cdf %>% 
  ggplot(aes(x=year, y=deaths_per_1000, colour = continent)) + 
  geom_line(data = grid, aes(y = pred),size=1.25) + 
  facet_wrap(~ model) +
  theme_bw() +
  labs(x="Year",
       y="Predicted U5MR",
       title = "Model Predictions")
p3

compare_performance(mod1,mod2,mod3) %>% plot

ggsave("./CHRISTENSEN_plot_3.png",p3)





predict(mod3)

