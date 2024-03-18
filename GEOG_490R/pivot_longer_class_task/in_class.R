# next tuesday ----

# pivot tables
# facet wrap

library(tidyverse)

#pivoting longer
billboard_long <- billboard %>% pivot_longer(cols = starts_with("wk"),
                           names_to = "week_number",
                           values_to = "rank")


#removing wk from week number
billboard_long$week_number = gsub("wk", "", billboard_long$week_number)

billboard_long$week_number = as.numeric(billboard_long$week_number)

billboard_long %>% 
  ggplot(aes(x = week_number, y = rank)) +
  geom_smooth() +
  labs(title = "Track Ranks Over Time",
       x = "Week Number",
       y = "Rank") +
  theme_bw()


# pivoting wider
us_rent_income_wide <- us_rent_income %>% pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  )

#creating new column that is rent over the year
us_rent_income_wide$rent_year = us_rent_income_wide$estimate_rent * 12



us_rent_income_wide %>% 
  ggplot(aes(x=NAME)) +
  geom_col(aes(y=estimate_income, fill = "Income")) +
  geom_col(aes(y=rent_year,fill = "Rent")) +
  labs(title = "Yearly Rent and Income Estimates by State",
       x = "State",
       y = "USD",
       fill = "Category") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

us_rent_income_wide %>% 
  ggplot(aes(x=reorder(NAME, -estimate_income))) +
  geom_col(aes(y=estimate_income, fill = "Income")) +
  geom_col(aes(y=rent_year,fill = "Rent")) +
  labs(title = "Yearly Rent and Income Estimates by State",
       x = "State",
       y = "USD",
       fill = "Category") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


us_rent_income_wide %>% 
  ggplot(aes(x=reorder(NAME, -rent_year))) +
  geom_col(aes(y=estimate_income, fill = "Income")) +
  geom_col(aes(y=rent_year,fill = "Rent")) +
  labs(title = "Yearly Rent and Income Estimates by State",
       x = "State",
       y = "USD",
       fill = "Category") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# create function that removes any NA values in a vector

vect <- c(sample(1:10),NA,sample(1:5),NA)

remove_na <- function(x){
  na_index = is.na(x)
  x[!na_index]}

remove_na(vect)






