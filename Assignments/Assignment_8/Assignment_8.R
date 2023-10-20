library(tidyverse)
library(janitor)
library(easystats)
library(modelr)

# load the “/Data/mushroom_growth.csv” data set
df <- read.csv("../../Data/mushroom_growth.csv") %>% 
  clean_names()

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
plot(m4)

# calculates the mean sq. error of each model
mean(m1$residuals^2)
mean(m2$residuals^2)
mean(m3$residuals^2)
mean(m4$residuals^2)
compare_performance(m1,m2,m3,m4) %>% plot
# selects the best model you tried
m3
# adds predictions based on new hypothetical values for the independent variables used in your model

pred <- df %>% 
  add_predictions(m3)

head(df)
x <- data.frame(species = c("P.ostreotus","P.cornucopiae"),
                humidity = c("Low","High"),
                light = c(10,35))
predict(m3,x)

# plots these predictions alongside the real data

# Upload responses to the following as a numbered plaintext document to Canvas:
#   Are any of your predicted response values from your best model scientifically meaningless? Explain.
# In your plots, did you find any non-linear relationships? Do a bit of research online and give a link to at least one resource explaining how to deal with modeling non-linear relationships in R.
# Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this)
  