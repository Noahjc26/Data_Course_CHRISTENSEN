library(tidyverse)
library(janitor)

#reading in ugly data from BIOL 3100 (We never worked with this so I thought it would be fun in this class)
ugly_data <- read.csv("./ugly_data2.csv")

print(ugly_data)

df1 <- (ugly_data[19:28,])
df2 <- (ugly_data[1:9,])

#setting correct column names
colnames(df1) <- df1[1,]

#removing top row now that it 
df1 <- (df1[-c(1),])

#adding sex column
df1$sex = "f"

#pivoting longer
df1 <- df1 %>% pivot_longer(starts_with("f"),
                     names_to = "age_range",
                     values_to = "count")

for (i in 1:nrow(df1)) {
  if (df1$age_range[i] == "f014") {
  df1$age_range[i] = "00-14"
} else if (df1$age_range[i] == "f1524"){
  df1$age_range[i] = "15-24"
} else{
  df1$age_range[i] = "35-44"
}
}


#adding sex column to df2
df2$sex = "m"

#pivoting longer
df2 <- df2 %>% pivot_longer(starts_with("m"),
                            names_to = "age_range",
                            values_to = "count")

for (i in 1:nrow(df2)) {
  if (df2$age_range[i] == "m014") {
    df2$age_range[i] = "00-14"
  } else if (df2$age_range[i] == "m1524"){
    df2$age_range[i] = "15-24"
  } else{
    df2$age_range[i] = "35-44"
  }
}


clean_data <- full_join(df2,df1)
