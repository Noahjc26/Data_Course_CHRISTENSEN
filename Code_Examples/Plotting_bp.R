library(tidyverse)
library(GGally)

dat <- readRDS("./Code_Examples/cleaned_bp.rds")

names(dat)

dat %>%
  ggplot(aes(x=Visit)) +
  geom_point(aes(y=systolic),color='red') +
  geom_point(aes(y=diastolic),color='black') +
  facet_wrap(~hispanic)

dat %>% 
  select(-c(pat_id,month_of_birth,day_birth,year_birth)) %>% 
            ggpairs(,cardinality_threshold = 33)

#control shift c to maker selected rows into comments
#select() picks columns and filter() picks rows
dat %>% 
  filter(race == "Black" & sex == "Female")

group_by()
summarize()

dat %>% 
  group_by(race,sex) %>% 
  summarize(mean_bp = mean(systolic))

arrange()
