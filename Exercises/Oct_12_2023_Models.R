library(tidyverse)
library(palmerpenguins)
library(modelr)
library(easystats)

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


#smaller aic the better your model is capturing reality
mod1$aic
mod2$aic
mod3$aic

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
df <- read.csv("../Data/GradSchool_Admissions.csv")
df <- df %>% 
  mutate(admit = as.logical(admit))

#logistic -> family = 'binomial'
mod6 <- glm(data = df, formula = admit ~ gre + gpa + rank,
            family = 'binomial')
summary(mod6)


add_predictions(df,mod6,type='response') %>% 
  ggplot(aes(x=gpa,y=pred,color=factor(rank))) +
  geom_smooth()
