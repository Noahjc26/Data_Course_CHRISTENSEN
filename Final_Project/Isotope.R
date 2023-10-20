library(tidyverse)
library(janitor)
library(ggpmisc)
library(devtools)
library(dplyr)
library(ggpubr)

x <- read.delim(pipe("pbpaste")) %>% 
  clean_names()
  
x %>% names

x2 <- subset(x, x$x39ar_released < 99.5)

x2 <- subset(x2, x$x39ar_released > 2)
p <- x %>% 
  ggplot(aes(x=x39ar_released,y=apparent_age)) +
    geom_line() +
  geom_line(data = x2, aes(x=x39ar_released, y=apparent_age), color="red") +
    theme_bw() +
    labs(x= "% of 39Ar Released",
         y= "Apparent Age (years)",
         title = "Age Spectrum")

p
p <- p + annotate(geom="text", x=50, y=950000000, label="plateau",size=5,
             color="black")
    p    

summary(x2$apparent_age)
sd(x2$apparent_age,na.rm=TRUE)
    ggsave("../age_spectrum_1_hw2_isotope.png",p)

    
    
    
#   Part 2 D) 
x <- read.delim(pipe("pbpaste")) %>% 
      clean_names()


p <- x %>%   
ggplot(aes(x=x39ar_40ar,y=x36ar_40ar)) +
  geom_smooth(method = 'lm',se=FALSE) +
  stat_regline_equation() +
  theme_bw() +
  labs( x= "39Ar/40Ar",
        y= "36Ar/40Ar",
        title = "Inverse Isochron")


sd(x$x36ar_40ar)
p

ggsave("../inverse_isochron_2_hw2_isotope.png",p)
    