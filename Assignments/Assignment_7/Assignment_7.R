library(tidyverse)
library(janitor)
library(GGally)

#reading in and cleaning data names
df <- read.csv("./Utah_Religions_by_County.csv") %>% 
  clean_names()


#1 multiplying all columns by pop_2010, while excluding columns "county" and "pop_2010"
#2 rounding all numeric values to whole numbers
#3 combining population of religions into one column
whole <- df %>% 
  mutate_at(vars(-c("county","pop_2010")), funs(.*pop_2010)) %>% #1
  mutate_if(is.numeric,round) %>% #2
  pivot_longer(cols = !c(county,pop_2010,religious), #3
               names_to = "religion",
               values_to = "pop") %>% 
  select(-religious)

percentages <- df %>% 
  mutate_at(vars(-c("county","pop_2010")), funs(.*100)) %>%
  pivot_longer(cols = !c(county,pop_2010,religious),
               names_to = "religion",
               values_to = "percentage") %>% 
  select(-religious)

full <- full_join(whole,percentages) %>% 
  rename(total_pop_2010 = pop_2010)


#Looking at each religion per county and their population
full %>% 
  ggplot(aes(x=reorder(county,-pop),y=pop,fill=religion)) +
  geom_col() +
  labs(y= "population", x = "county") +
  theme(axis.text.x = element_text(angle = 90))

#looking at the total population of all the religions in all the counties
full %>% 
  ggplot(aes(x=reorder(religion,-pop),y=pop,fill=religion)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))


#Does population of a county correlate with the 
#proportion of any specific religious group in that county?

full %>% 
  ggplot(aes(x=total_pop_2010,y=percentage,fill=religion)) +
  geom_area() +
  labs(y= "percentage of population", x = "population of county") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  theme_minimal()
#Looking at the graph above it seems like there is no strong correlation between population 
#of a county and any specific religious group. 


#“Does proportion of any specific religion in a given 
#county correlate with the proportion of non-religious people?”

full %>% 
  ggplot(aes(x=religion,y=percentage,fill=religion)) +
  geom_col() +
  labs(y= "population", x = "county") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  facet_wrap(~county)

#it definitely seems like when there are more LDS then there are less non-religious (except in summit county)
#however, since this is a percentage comparison, a higher amount in one will ultimately mean a lower amount in the other categories.



