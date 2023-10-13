library(tidyverse)
library(janitor)

x <- read.delim(pipe("pbpaste")) %>% 
  clean_names()
  
x %>% names

x2 <- subset(x, x$apparent_age < 1000000000)

p <- x %>% 
  ggplot(aes(x=x39ar_released,y=apparent_age)) +
    geom_line() +
  geom_line(data = x2, aes(x=x39ar_released, y=apparent_age), color="red") +
    theme_bw() +
    labs(x= "% of 39Ar Released",
         y= "Apparent Age (years)",
         title = "Age Spectrum")


p <- p + annotate(geom="text", x=50, y=950000000, label="plateau",size=5,
             color="black")
    p    

summary(x2$apparent_age)
sd(x2$apparent_age)
    ggsave("../../age_spectrum_1_hw2_isotope.png",p)
