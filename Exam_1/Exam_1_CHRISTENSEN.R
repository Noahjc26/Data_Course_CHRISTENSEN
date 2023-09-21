library(tidyverse)


#1 Read into R Data Frame
CCD <- read.csv("./cleaned_covid_data.csv")

#2 Subset into data with only states starting with A

A_states<- subset(CCD, grepl("^A",Province_State))


#3 Plot
class(A_states$Last_Update)

ggplot(data=A_states,
       aes(x=ymd(Last_Update),
           y=Deaths,
           )) +
       geom_point(alpha=.5,
                  size=.2) +
  facet_wrap(~Province_State,
             scales="free") +
  geom_smooth(method = "lm",
              se = FALSE,
              ) +
  labs(x="DATE (year-month-day)",
       y="Number of Deaths")

#4 Peak Case_Fatality_Ratio

#creating first temporary data frame to find max Case_Fatality_Ratio, removing NA, and changing column name

fatality_rate_temp1 <- CCD %>%
  group_by(Province_State) %>%
  filter(Case_Fatality_Ratio == max(Case_Fatality_Ratio, na.rm=TRUE)) %>%
  rename("Maximum_Fatality_Ratio" = "Case_Fatality_Ratio")

#creating second temporary data frame to remove any duplicate states
fatality_rate_temp2 <- fatality_rate_temp1[!duplicated(fatality_rate_temp1$Province_State),]

#creating third temporary data frame to sort by Maximum_Fatality_Ratio in descending order
fatality_rate_temp3 <- fatality_rate_temp2[order(-fatality_rate_temp2$Maximum_Fatality_Ratio),]

#creating final data frame with only the two columns "Province_State" and "Maximum_Fatality_Ratio"
state_max_fatality_rate = fatality_rate_temp3[c("Province_State","Maximum_Fatality_Ratio")]

# how Zahn did it with for loop
#peaks <- c()

#for(i in unique(df$Province_State)){
 # x <- df[df$Province_State == i,]
  #y <- max(x$Case_Fatality_Ratio, na.rm = TRUE)
#  peaks[i] <- y
#}

#data.frame(State = names(peaks),
 #          Peak = peaks)
#easy way
#state_max_fatality_rate <- 
#df %>%
 # group_by(Province_State) %>%
  #summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio,na.rm=TRUE)) %>%
  #arrange(desc(Maximum_Fatality_Ratio))

#5

#factors
# state_max_fatality_rate$Province_State <- 
 # factor(state_max_fatality_rate$Province_State,
  #       levels = state_max_fatality_rate$Province_State)

ggplot(state_max_fatality_rate,
       aes(x=reorder(Province_State, -Maximum_Fatality_Ratio),
           y=Maximum_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) 

#6 maybe
#CCD %>%
#  group_by(Last_Update) %>%
#  summarize(cumulative_deaths = sum(Deaths,na.rm=TRUE)) %>%
#  ggplot(aes(x=Last_Update,y=cumulative_deaths)) +
#  geom_smooth()
