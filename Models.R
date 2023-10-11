library(tidyverse)
library(modelr)

mpg %>% names

mpg %>% 
  ggplot(aes(x=displ,y=hwy,color=factor(cyl))) +
  geom_smooth(method='lm')
mpg$hwy %>% mean


#generalized linear model regression using mpg data frame, hwy as a function of displacement
m <- glm(data = mpg,
    formula = hwy ~ displ + factor(cyl))

summary(m)


m$coefficients


preds <- add_predictions(mpg,m)




