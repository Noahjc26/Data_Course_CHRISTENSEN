library(tidyverse)
library(janitor)
library(easystats)
library(modelr)

# load the “/Data/mushroom_growth.csv” data set
df <- read.csv("../../Data/mushroom_growth.csv")

df <- df %>% janitor::clean_names()
# creates several plots exploring relationships between the response and predictors
df %>% 
  ggplot(aes(y=growth_rate, x=humidity, fill=species)) +
  geom_boxplot()

df %>% 
  ggplot(aes(y=growth_rate, x=light, fill=species)) +
  geom_smooth()

df %>% 
  ggplot(aes(y=growth_rate, x=nitrogen, fill=species)) +
  geom_smooth()


# defines at least 4 models that explain the dependent variable “GrowthRate”
m1 <- lm(data = df, formula = growth_rate ~ temperature + humidity)
m2 <- lm(data = df, formula = growth_rate ~ temperature * humidity)
m3 <- lm(data = df, formula = growth_rate ~ species * humidity * light * temperature)
m4 <- lm(data = df, formula = growth_rate ~ temperature * humidity * species)


# calculates the mean sq. error of each model
mean(m1$residuals^2)
mean(m2$residuals^2)
mean(m3$residuals^2)
mean(m4$residuals^2)
compare_performance(m1,m2,m3,m4) %>% plot
# selects the best model you tried
m3
# adds predictions based on new hypothetical values for the independent variables used in your model

df <- df %>% 
  add_predictions(m3)

x <- data.frame(species = c("P.ostreotus","P.cornucopiae","P.cornucopiae","P.cornucopiae"),
                humidity = c("Low","High","High","High"),
                light = c(10,35,50,50),
                temperature = c(10,20,0,-10))

hypo <- predict(m3,x)


# combining hypothetical input data with hypothetical predictions into one new data frame
hyp_preds <- data.frame(species = x$species,
                        light = x$light,
                        humidity = x$humidity,
                        temperature = x$temperature,
                        pred = hypo)

# Add new column showing whether a data point is real or hypothetical
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

# joining our real data and hypothetical data (with model predictions)
fullpreds <- full_join(df,hyp_preds)


# plotting temperature compared to predictions
ggplot(fullpreds,aes(x=temperature,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=growth_rate),color="Black") +
  theme_minimal()

# plotting light compared to predictions
ggplot(fullpreds,aes(x=light,y=pred,color=PredictionType)) +
  geom_point(aes(y=growth_rate),color="Black") +
  geom_point() +
  theme_minimal()

# Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this)

non <- read.csv("../../Data/non_linear_relationship.csv")


non %>% 
  ggplot(aes(x=(predictor^3),y=(response))) +
  geom_point() +
  geom_smooth()

