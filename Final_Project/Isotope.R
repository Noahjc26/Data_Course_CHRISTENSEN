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

x2 <- subset(x2, x$x39ar_released > 15)
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
ggplot(aes(y=x39ar_40ar,x=x36ar_40ar)) +
  geom_point() +
  geom_smooth(method = 'lm',se=FALSE) +
  stat_regline_equation() +
  theme_bw() +
  labs( x= "39Ar/40Ar",
        y= "36Ar/40Ar",
        title = "Inverse Isochron")


sd(x$x36ar_40ar)
p

ggsave("../inverse_isochron_2_hw2_isotope.png",p)






df <- data.frame(biotite = c("674-1","579-11","529-9"),
           inverse_isochron_age = c(38.7,37.7,35),
           age_spectrum_age = c(38.4,37.9,37.7))

df <- 
  pivot_longer(df,cols = c(inverse_isochron_age,age_spectrum_age),
             values_to = "age_Ma",
             names_to = "age_type") %>% 
  mutate(sd = 0.2)


pp <- df %>% 
  ggplot(aes(x=biotite,y=age_Ma,color=age_type)) +
  geom_errorbar(aes(ymin=age_Ma-sd, ymax=age_Ma+sd), width=.2,
                position=position_dodge(0)) +
  geom_point() +
  theme_bw() +
  labs(title = "Zircon Ages",
       x = "Biotite Number",
       y = "Age in Ma")

ggsave("../../../ZirconAges.png",pp)




part7 <- read.delim(pipe("pbpaste")) %>% 
  clean_names()

part7_calculated <- subset(part7,subset = in_samples == TRUE)
part7_not <- subset(part7,subset = in_samples == FALSE)

ppp <- ggplot(data=part7,aes(y=x206pb_238u,x=x207pb_235u)) +
  geom_smooth(data=part7_calculated,method='lm',se=FALSE,fullrange=TRUE) +
  geom_point(data=part7_calculated,color='red') +
  stat_regline_equation()+
  geom_point(data = part7_not) +
  ylim(0,2) +
  theme_bw() +
  labs(x="207Pb/235U",
       y="206Pb/238U",
       title = "Ratios and linear regression plotted on concordia")

ggsave("../../../Part7.png",ppp)


part8 <- read_delim(pipe("pbpaste")) %>% 
  clean_names()

part8
pppp<- part8 %>% 
  ggplot(aes(x=depth_mm,y=ln_be)) +
  geom_point() +
  geom_smooth(method = 'lm',se=FALSE) +
  stat_regline_equation() +
  labs(x="Depth in mm",
       y="ln(10Be dpm/kg)",
       title = "Mn nodule concentrations at depths") +
  theme_bw()
pppp
ggsave("../../../Part8.png",pppp)
