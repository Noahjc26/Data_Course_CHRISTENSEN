# Assignment 9
# In this assignment, you will use R (within R-Studio) to:
#   
#   Load and clean a real data set
# Conduct exploratory analyses, including informative figures
# Build and test appropriate models
# Draw conclusions about your data
# Combine all of the above into a well-documented R-Markdown report and export (knit) it into an HTML file
# All file paths should be relative, starting from your Assignment_9 directory!!
#   
#   This means that you need to create a new R-Project named “Assignment_9.Rproj” in your Assignment_9 directory, and work from scripts within that.
# 
# For credit…
# Push a completed version of your Rproj and R-markdown file (details at end of this assignment) to GitHub
# Your score will also depend on your analyses and presentation of your final report
# Your tasks:
#   Use the data set “/Data/GradSchool_Admissions.csv”
# You will explore and model the predictors of graduate school admission
# the “admit” column is coded as 1=success and 0=failure (that’s binary, so model appropriately)
# the other columns are the GRE score, the GPA, and the rank of the undergraduate institution, where I is “top-tier.”
# Document your data explorations, figures, and conclusions in a reproducible R-markdown report
# That means I want to see, in your html report, your process of model evaluation and selection. Here’s an example
# Upload your self-contained R project, including knitted HTML report, to GitHub in your Assignment_9 directory

library(tidyverse)
library(janitor)
library(GGally)
library(modelr)
library(easystats)
library(huxtable)
library(MASS)
df <- read.csv("../../Data/GradSchool_Admissions.csv") %>% 
  janirot::clean_names()

df <- df %>% janitor::clean_names()

#changing 0 and 1 to true and fales
df <- df %>% 
  mutate(admit = as.logical(admit))

#viewing pairs
ggpairs(df)

#plotting
df %>% 
  ggplot(aes(x=gre,y=admit)) +
  geom_boxplot() +
  facet_wrap(~rank) +
  theme_bw()

df %>% 
  ggplot(aes(x=gpa,y=admit)) +
  geom_boxplot() +
  facet_wrap(~rank) +
  theme_bw()

#viewing names in data frame
names(df)

#making models
m1 <- glm(data = df,
          admit ~ gre,
          family = 'binomial')
m2 <- glm(data = df,
          admit ~ gpa,
          family = 'binomial')
m3 <- glm(data = df,
          admit ~ rank,
          family = 'binomial')
m4 <- glm(data = df,
          admit ~ gre + gpa + rank,
          family = 'binomial')
m5 <- glm(data = df,
          admit ~ gre * gpa * rank,
          family = 'binomial')
m6 <- glm(data = df,
          admit ~ .^2,
          family = 'binomial')

#feed largest most complicated model and it will find the best one
step <- stepAIC(m6)

#best simplified model for this data set
step$formula

bestmod <- glm(data = df,
          admit ~ gre + gpa + rank + gre:gpa,
          family = 'binomial')

#comparing performance of models

comparison <- compare_performance(m1,m2,m3,m4,m5,m6,m7,rank=TRUE)
comparison
comparison %>% plot


huxreg(list("Model 1" = m1, "Model 2" = m2, "Model 3" = m3, "Model 4" = m4, "Model 5" = m5, "Best Model" = bestmod))


add_predictions(df,bestmod,type='response') %>% 
  ggplot(aes(x=gpa,y=pred,color=factor(rank))) +
  geom_smooth()

add_predictions(df,bestmod,type='response') %>% 
  ggplot(aes(x=gre,y=pred,color=factor(rank))) +
  geom_smooth()

add_predictions(df,bestmod,type='response') %>% 
  ggplot(aes(x=rank,y=pred)) +
  geom_smooth()


