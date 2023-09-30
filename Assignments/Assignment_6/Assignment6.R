library(tidyverse)
library(janitor)
library(gganimate)
df <- read.csv("../../Data/BioLog_Plate_Data.csv")
head(df)

clean_names(df)
#turn hours into variable
cleaneddf <- df %>% 
  pivot_longer(cols = c(starts_with("Hr_")),
               names_to = "Time",
               names_prefix = "Hr_",
               names_transform = as.numeric,
               values_to = "Absorbance") %>% 
  mutate(Type = Sample.ID,
         Type = case_when(Type == "Clear_Creek" ~ "Water",
                              Type == "Waste_Water" ~ "Water",
                              Type == "Soil_1" ~ "Soil",
                              Type == "Soil_2" ~ "Soil"))

subdf <- filter(cleaneddf,cleaneddf$Dilution == "0.1")

subdf %>% 
ggplot(aes(x=Time,y=Absorbance,color=Type)) +
  geom_smooth(se = FALSE,method = loess) +
  ylim(0,2) +
  facet_wrap(~Substrate) +
  theme_minimal()


subdf2 <- filter(cleaneddf,cleaneddf$Substrate == "Itaconic Acid")

subdf3 <- subdf2 %>% group_by(Time,Sample.ID,Dilution) %>% 
  mutate(Absorbance_mean = mean(Absorbance)) %>% 
  distinct(Absorbance_mean,Time)

subdf3 %>% 
  ggplot(aes(x=Time,y=Absorbance_mean,color=Sample.ID)) +
  geom_line() +
  facet_wrap(~Dilution) +
  transition_reveal(Time)

