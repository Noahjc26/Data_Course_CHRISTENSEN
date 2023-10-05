library(tidyverse)
library(janitor)
library(readxl)

path <- "./Data/messy_bp.xlsx"

visits <- read_xlsx(path, skip = 2, n_max = 0) %>% names()# skipping 2 rows and setting max number of rows to 1

df <- read_xlsx(path, range = "A4:M24") %>% 
  clean_names()

#df %>% 
#pivot_longer(starts_with("bp_"),
 #            values_to = "bp") %>% 
  #mutate(Visit = case_when(name == "bp_8" ~ 1,
   #                       name == "bp_10" ~ 2,
    #                       name == "bp_12" ~ 3)) %>% 
#  pivot_longer(starts_with("hr_"),
 #              names_to = "visit2",
  #             values_to = "heart_rate")
bp <- 
df %>% 
  select(-starts_with("hr_")) %>% #for picking columns
  pivot_longer(starts_with("bp_"),
               values_to = "bp") %>% 
  mutate(Visit = case_when(name == "bp_8" ~ 1,
                           name == "bp_10" ~ 2,
                           name == "bp_12" ~ 3)) %>% 
  select(-name) %>% 
  separate(bp, into = c("systolic","diastolic"), convert = TRUE) #(convert) turning them into numeric values

hr <- 
df %>% 
  select(-starts_with("bp_")) %>% 
  pivot_longer(starts_with("hr_"),
               values_to = "hr") %>% 
  mutate(Visit = case_when(name == "hr_9" ~ 1,
                           name == "hr_11" ~ 2,
                           name == "hr_13" ~ 3)) %>% 
  select(-name)

dat <- 
full_join(bp,hr) #joining by all the ones in common

dat <- 
dat %>% 
  mutate(birthdate = paste(year_birth,
                           month_of_birth,
                           day_birth,
                           sep="-") %>% 
           as.POSIXct()) #posixct is turning it into an actual date

dat <- dat %>% 
  mutate(race = case_when(race == "WHITE" ~ "White",
                          race == "Caucasian" ~ "White",
                          TRUE ~ race)) #making it so it keeps all other stuff the same

saveRDS(dat,"./Code_Examples/cleaned_bp.rds") #r data structure file, saves df and stuff in environment

#---------------------------------------------------------------------------------------------------




