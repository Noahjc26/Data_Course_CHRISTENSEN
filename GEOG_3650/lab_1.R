library(tidyverse)
library(ggmisc)
library(janitor)

utah_2021 <- read.csv2("../../../OneDrive - Utah Valley University/GEOG_3650/GEOG_3650_Lab_1/CSV/UtahData_2021.csv")
utah_2022 <- read.csv2("../../../OneDrive - Utah Valley University/GEOG_3650/GEOG_3650_Lab_1/CSV/UtahData_2022.csv")
utah_normal <- read.csv("../../../OneDrive - Utah Valley University/GEOG_3650/GEOG_3650_Lab_1/CSV/Utahnormals.csv")

utah_normal <- utah_normal %>% clean_names() %>% 
  mutate(year = "normal") %>% 
  rename(annual_precip = ann_prcp_normal)

utah_2021 <- utah_2021 %>% clean_names() %>% 
  mutate(year = "2021") %>% 
  rename(annual_precip = x2021)

utah_2022 <- utah_2022 %>% clean_names() %>% 
  mutate(year = "2022") %>% 
  rename(annual_precip = x2022)


a <- full_join(utah_normal,utah_2021)

full_df <- full_join(a,utah_2022)

full_df <- full_df %>% 
  select(-c(x_1,x))

full_df %>% 
  ggplot(aes(x=station,y=annual_precip,color=year)) +
  geom_point()

summary(utah_2022$annual_precip)
summary(utah_2021$annual_precip)
summary(utah_normal$annual_precip)



# Function to calculate slope and R^2
calculate_stats <- function(data) {
  lm_model <- lm(annual_precip ~ elevation, data = data)
  slope <- coef(lm_model)[2]
  r_squared <- summary(lm_model)$r.squared
  return(data.frame(slope = slope, r_squared = r_squared))
}

# Calculate stats for each facet
facet_stats <- full_df %>%
  group_by(year) %>%
  do(calculate_stats(.))

# Merge stats back into the original data frame
full_df <- merge(full_df, facet_stats, by = "year")

# Plotting
p <- full_df %>%
  ggplot(aes(x = elevation, y = annual_precip)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~year) +
  labs(y= "Annual Precipitation",
       x = "Elevation") +
  theme_linedraw()

# Add text annotation for slope and R^2
p + geom_text(
  data = facet_stats,
  aes(x = Inf, y = -Inf, label = sprintf("Slope: %.2f\nR^2: %.2f", slope, r_squared)),
  hjust = 1.1, vjust = -0.2, size = 3
)
  
