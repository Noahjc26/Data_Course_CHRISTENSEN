library(tidyverse)
library(palmerpenguins)
library(modelr)
library(easystats)
library(MASS)

penguins
penguins %>% 
  ggplot(aes(y=bill_depth_mm,x=bill_length_mm,color=species)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~sex)
 # coord_cartesian(xlim = c(0,60))


mod1 <- glm(data = penguins,
            formula = bill_depth_mm ~ bill_length_mm)


#linear regression for this, could also use "aov" in place of "glm"
mod2 <- glm(data = penguins,
    formula = bill_depth_mm ~ bill_length_mm + species)

#multiply sign allows for individual y=mx+b for each species
mod3 <- glm(data = penguins,
    formula = bill_depth_mm ~ bill_length_mm * species)


mod4 <- glm(data = penguins,
            formula = bill_length_mm ~ species * sex * body_mass_g * flipper_length_mm * bill_depth_mm * island * year)


#creating best fit model
full_mod <- glm(data=penguins,
                formula = bill_length_mm ~ .^2)
pbest <- stepAIC(full_mod)
pbest$formula
modbest <- glm(data = penguins,formula=pbest$formula)
compare_performance(full_mod,modbest) %>% plot


#train model on some data
#test it on other data

dim(penguins)


rbinom(nrow(penguins),1,.8)

#add new column with true/false at 80% true
penguins <- penguins %>% 
  mutate(newcol = rbinom(nrow(penguins),1,.8))

#split into train and test models
train <- penguins %>% filter(newcol == 1)
test <- penguins %>% filter(newcol == 0)

#make model
mod_best <- glm(data=train, formula = pbest$formula)

#test model on test set
predictions <- 
add_predictions(test,mod_best)

#calculating the absolute difference between the actual and prediction
predictions <- predictions %>% 
  mutate(resid = abs(pred - bill_length_mm))

#calculating mean of resid (pred - bill_length_mm)
mean_err <- mean(predictions$resid,na.rm = TRUE)

#cross-validation of models


library(ranger)

ranger_mod <- ranger(Species ~ ., data = iris)

#would not do it with ^2   !!!
ranger(Species ~ .^2,data = iris)

pred <- predict(ranger_mod,iris)

data.frame(iris$Species,pred$predictions)


penguins
#smaller aic the better your model is capturing reality
mod1$aic
mod2$aic
mod3$aic
full_mod$aic
modbest$aic

compare_performance(mod1,mod2,mod3) %>% plot

#adding sex as a comparison
mod4 <- glm(data=penguins,
            formula= bill_depth_mm ~ bill_length_mm * species + sex + island)


#mod4 is much better than the rest
compare_performance(mod1,mod2,mod3,mod4) %>% plot

formula(mod4)

x <- data.frame(bill_length_mm = c(5000,100),
           species = c("Adelie","Chinstrap"),
           sex = c("male","male"),
           island = c("Dream","Dream"))

predict(mod4,newdata = x)

mpg %>% 
  ggplot(aes(x=displ,y=hwy,color=factor(cyl))) +
  geom_point() +
  geom_smooth(method='lm',formula = y~ log(x))


y <- data.frame(displ = 500)
mod5 <- glm(data=mpg,formula = hwy ~ log(displ))

10^predict(mod5,y)



#predicting binary outcomes (true,false) instead of continuous data

#logistic regression (outcome is true false)
df <- read.csv("../../Data/GradSchool_Admissions.csv")
df <- df %>% 
  mutate(admit = as.logical(admit))

#logistic -> family = 'binomial'
mod6 <- glm(data = df, formula = admit ~ gre * gpa * rank,
            family = 'binomial')
summary(mod6)

#feed largest most complicated model and it will find the best one
step <- stepAIC(mod6)
#best simplified model for this data set
step$formula


mod7 <- glm(data = df, formula = admit ~ gre + gpa + rank,
            family = 'binomial')

compare_performance(mod6,mod7) %>% plot


add_predictions(df,mod6,type='response') %>% 
  ggplot(aes(x=gpa,y=pred,color=factor(rank))) +
  geom_smooth()

modbest <- glm(data = df,family = "binomial",formula=step$formula)
compare_performance(mod6,mod7,modbest) %>% plot


