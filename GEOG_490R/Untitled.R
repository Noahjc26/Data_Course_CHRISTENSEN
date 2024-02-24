library(tidyverse)

revenue <- data.frame(
  cid = c(1, 2, 3),
  names = c("Odile", "Rasheda", "Ettie"),
  age = c(7, 2, 25),
  revenue.2016 = c(272, 478, 614),
  revenue.2017 = c(345, 788, 655),
  revenue.2018 = c(495, 390, 702)
)

?pivot_longer

revenue_long <- revenue %>% 
  pivot_longer(cols = c("revenue.2016","revenue.2017","revenue.2018"),
               names_to = "year",
               values_to = "revenue")

revenue_long %>% 
  ggplot(aes(x=names,y=revenue, color = year)) +
  geom_point() +
  facet_grid(~year)
  


billboard_long <- billboard %>% 
  pivot_longer(cols = starts_with("wk"),
               names_to = "week",
               values_to = "rank")

billboard_long$week <- gsub("wk","",billboard_long$week) %>% 
  as.numeric()

billboard_long %>% 
  ggplot(aes(x=week,y=rank)) +
  geom_smooth()


us_rent_income



# Sample dataframe for the type of bird for each state in the United States
state_birds <- data.frame(
  State = c("alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming"),
  Bird = c("Yellowhammer", "Willow Ptarmigan", "Cactus Wren", "Northern Mockingbird", "California Quail", "Lark Bunting", "American Robin", 
           "Delaware Blue Hen", "Northern Mockingbird", "Brown Thrasher", "Nene (Hawaiian Goose)", "Mountain Bluebird", "Northern Cardinal", 
           "Northern Cardinal", "Eastern Goldfinch", "Eastern Goldfinch", "Western Meadowlark", "Northern Cardinal", "Chickadee", 
           "Chickadee", "Black-Capped Chickadee", "American Robin", "American Robin", "Northern Mockingbird", "Eastern Bluebird", 
           "Western Meadowlark", "Western Meadowlark", "Western Meadowlark", "Mountain Bluebird", "Purple Finch", "Eastern Goldfinch", 
           "Eastern Goldfinch", "Roadrunner", "Eastern Bluebird", "Western Meadowlark", "Cardinal", "Scissor-Tailed Flycatcher", 
           "Western Meadowlark", "Ruffed Grouse", "Rhode Island Red", "Carolina Wren", "Ring-Necked Pheasant", "Northern Mockingbird", 
           "Northern Mockingbird", "Northern Mockingbird", "Hermit Thrush", "Cardinal", "American Robin", "American Robin", "Western Meadowlark" 
  )
)


state_flags <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
            "West Virginia", "Wisconsin", "Wyoming"),
  Flag_Color = c("Red, White", "Blue, Gold", "Blue, Gold", "Red, White", "Blue, Gold", "Red, White", "Blue, White", "Blue, Yellow", 
                 "Red, White", "Red, White", "Red, White", "Red, Blue", "Blue, White", "Blue, Gold", "Red, White", "Red, White", 
                 "Blue, Yellow", "Blue, Gold", "Blue, White", "Blue, Yellow", "Red, White", "Blue, Gold", "Blue, Gold", "Blue, Yellow", 
                 "Red, White", "Red, White", "Blue, Gold", "Blue, Gold", "Blue, Silver", "Blue, Gold", "Buff, Blue", "Red, Yellow", 
                 "Red, Yellow", "Blue, Gold", "Red, White", "Green, Yellow", "Red, White", "Blue, White", "Blue, Gold", "Red, White", 
                 "White, Blue", "Blue, White", "Blue, Gold", "Red, White", "Red, White", "Red, White", "Red, White", "Red, White", 
                 "Red, White, Blue", "Red, Yellow")
)

sample()



merged_data <- full_join(state_flags, state_birds, by = "State")
print(merged_data)

matchedrows <- match(state_birds$State,state_flags$State)


jazz_2020 <- data.frame(
  Player = c("Player1", "Player2", "Player3"),
  Points = c(20, 15, 18),
  Assists = c(6, 5, 7),
  Rebounds = c(8, 7, 9),
  ThreePointPercentage = c(0.35, 0.42, 0.38)
)

jazz_2022 <- data.frame(
  Player = c("Player1", "Player2", "Player3"),
  Points = c(22, 18, 20),
  Assists = c(7, 6, 8),
  Rebounds = c(9, 8, 10),
  ThreePointPercentage = c(0.37, 0.41, 0.39)
)

?match
combined_jazz <- cbind(jazz_2020, jazz_2022[, -1])
