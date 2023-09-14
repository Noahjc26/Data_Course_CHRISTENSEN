library(tidyverse)

#1 Read into R Data Frame
CCD <- read.csv("./Exam_1/cleaned_covid_data.csv")

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

#5



X-axis is Province_State
Y-axis is Maximum_Fatality_Ratio
bar plot
x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
X-axis labels turned to 90 deg to be readable
Even with this partial data set (not current), you should be able to see that (within these dates), different states had very different fatality ratios.

VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time

You’ll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.
