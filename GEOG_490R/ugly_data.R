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


write.csv(df1,"./f_count.csv")
write.csv(df2,"./m_count.csv")



clean_data <- full_join(df2,df1,by = join_by(Year, sex, age_range, count))

clean_data$count <- as.numeric(clean_data$count)

clean_data %>% 
  ggplot(aes(x=age_range,y=count,fill = sex)) +
  geom_col()



tot <- read.csv("../tot_migrant_deaths_2023.csv")



# This function is very specific and only works with this dataset... BUT if this dataset were to be updated you could easily use this function to create plots showing death count and cause of death in different counties and visiualizing the difference in deaths by sex.
plot_death <- function(x){
  y <- x[x$Sex != "undetermined",] %>% 
    ggplot(aes(x=Cause.of.Death,fill=Sex)) +
    geom_bar() +
    theme_bw() +
    labs(x="Cause of death",
         y= "Count")

  
  y + theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1)) +
    facet_wrap(~County)
}


plot_death(tot)


tot <- read.csv("./tot_migrant_deaths_2023.csv")

plot_death <- function(x){y <- x[x$Sex != "undetermined",] %>%  ggplot(aes(x=Cause.of.Death,fill=Sex)) + geom_bar() + theme_bw() +  labs(x="Cause of death",  y= "Count") 
y + theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1)) + facet_wrap(~County)}

plot_death(tot)
