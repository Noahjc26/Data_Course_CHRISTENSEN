##### Part 1 #####
# 1. Data types and structures
  
# Find the number of days in the semester.
start_date <-  as.Date("01/08/2024",tryFormats = "%m/%d/%Y")
end_date <- as.Date("04/23/2024",tryFormats = "%m/%d/%Y")

time_frame <- end_date - start_date
print(time_frame)

#Extract all dates that fall on a Saturday and count the total number of Saturdays in the semester.
all_dates <- seq(start_date, end_date, by = "days")

# Extract all dates that fall on a Saturday
saturdays <- all_dates[weekdays(all_dates) == "Saturday"]

#Round up and assume that we meet from 1-4PM (3 hrs) each day for this class. Use the dates of the semester and these details to calculate the number of hours you will spend in this class this semester. (no need to consider holidays)

meeting_days <- all_dates[weekdays(all_dates) == "Tuesday" | weekdays(all_dates) == "Thursday"]

#This is how many hours we will meet throughout the semester
length(meeting_days) * 3

# emails
emails <- c("w2u9q0g7z6@outlook.com", "m7n4w6f1a0@outlook.com", "withspaces@gmail.com",  
              "n7y4h5k0t6@gmail.com", "@invalid", "e2m5h7q1z0@outlook.com", 
              "invalid@", "invalid@", "invalid@",
              "g2b1j5u7o8@outlook.com", "s1c8k0t6e7@gmail.com", "f9m1a4z8h6@outlook.com",
              "e8c5f3v6k9@yahoo.com", "missingdotgmail.com", "invalid@email",
              "b6g3f0y4d2@gmail.com", "k0p8i9s6o3@outlook.com", "s8t7g2k9o5@yahoo.com", 
              "a9i7y1w5q0@gmail.com", "r2g9y4l5i6@yahoo.com", "b7d4a5u0j2@outlook.com",
              "l0a7z9m6r1@outlook.com", "c4v2a7w6r1@yahoo.com", "x1s3d8y5c6@yahoo.com",
              "@invalid", "@invalid", "@invalid",
              "@invalid", "invalid@", "e3k9x2m8t4@gmail.com",
              "invalidemail", "k2a3b7l1n8@outlook.com", "d0a6q9u2z4@yahoo.com",
              "i0f2z8c5x1@outlook.com", "q4m0l5o6j2@outlook.com", "v5h9a4f1q0@outlook.com",
              "@invalid", "f0z3h5d1v2@gmail.com", "g7k0u3a5s6@gmail.com",
              "@invalid", "invalid@", "@invalid", 
              "@invalid", "n9t0h3e6i5@outlook.com", "l2k1r6z0g5@outlook.com",
              "f9d7u5e1n8@outlook.com", "n9o4s8a6b0@yahoo.com", "@invalid",
              "s1b8q5i9z2@gmail.com", "z7v3n1o0f9@outlook.com")

#creating new vector with emails that contain the @ sign
emails_with_at_sign <- emails[grep("@",emails)]

#using the @ sign vector and keeping only the emails with .com
valid_emails <- emails_with_at_sign[grep(".com",emails_with_at_sign)]

valid_emails

# phone numbers
phones <- c("(525) 692-8788", "(748) 654-2046", "(865) 970-8066", "(310) 472-4003", 
            "(689) 943-4206", "920.680.4936", "323.171.5088", "265.687.3906", 
            "316.674.1293", "389.240.9468", "562 322 3503", "140 627 7741", 
            "530 215 9943", "189 705 9565", "415 873 7128", "133-854-8126", 
            "615-508-4357", "112-407-4979", "168-377-4229", "994-188-6602",
            "123-456-789", "abc-123-4567", "123 45 6789", "1a2b3c4d5e", 
            "12345", "67890", "1234", "6789", "12345678903", "12345678901")

#cleaning up phone numbers
#removing hyphens
phones <- gsub("-","", phones)

#removing dots
phones <- gsub("\\.","",phones)

#removing parenthesis
phones <- gsub("[()]","",phones)

#removing spaces
phones <- gsub(" ","",phones)

#replacing any a,b,or c with 2
phones <- gsub("[abc]","2",phones)

#replacing any d,e,or f with 3
phones <- gsub("[def]","3",phones)

#replacing any g,h,or i with 4
phones <- gsub("[ghi]","4",phones)

#replacing any j,k or l with 5
phones <- gsub("[jkl]","5",phones)

#replacing any m,n,or o with 6
phones <- gsub("[mno]","6",phones)

#replacing any p,q,r,or s with 7
phones <- gsub("[pqrs]","7",phones)

#replacing any t,u,v with 8
phones <- gsub("[tuv]","8",phones)

#replacing any w,x,y,or z with 9
phones <- gsub("[wxyz]","9",phones)

#use nchar function which tells the number of characters for each element in the vector
character_lengths <- nchar(phones)

#subset to keep only elements with 10 characters
valid_phones <- phones[character_lengths == 10]

valid_phones

#adding underscores after the first 3 and the first 6 characters
valid_phones <- gsub("^(.{3})(.{3})", "\\1_\\2_", valid_phones)

valid_phones

#checking in phone numbers are valid

#making a pattern that is xxx_xxx_xxxx
pattern <- "^\\d{3}_\\d{3}_\\d{4}$"

#using grepl to see boolean values if they follow that pattern
grepl(pattern, valid_phones)


##### Part 2 #####
#2. Snotel data

# Read in csv file provided in canvas
snotel <- read.csv("./snotel820_timp.csv")

# After reading in the data, convert dates column to a date format
snotel$date <- as.Date(snotel$date)

#estimating the density of the snowpack each day
snotel$density = snotel$swe/snotel$depth * 100

# List the average and standard deviation of the following: 
#snow density (in percent)
mean_density <- round((mean(snotel$density)), 2)
sd_density <- round((sd(snotel$density)), 2)

#snow depth
mean_depth <- round((mean(snotel$depth)),2)
sd_depth <- round((sd(snotel$depth)),2)

#average temperature
mean_ave_temp <- round((mean(na.omit(snotel$t_ave))),2)
sd_ave_temp <- round((sd(na.omit(snotel$t_ave))),2)

#warmest and coldest temperature recorded during the time period
max_temp <- max(na.omit(snotel$t_max))
min_temp <- min(na.omit(snotel$t_min))

#Concatenate this information into a single print or cat statement
cat("Density Average = ", mean_density, "%",
                    "\n",
                    "Density Standard Deviation = ", sd_density, "%",
                    "\n",
                    "Depth Average = ", mean_depth, '"',
                    "\n",
                    "Depth Standard Deviation = ", sd_depth,'"',
                    "\n",
                    "Temperature Average = ", mean_ave_temp, "°F",
                    "\n",
                    "Temperature Standard Deviation = ", sd_ave_temp, "°F",
                    "\n",
                    "Max Temperature Recorded = ", max_temp, "°F",
                    "\n",
                    "Minimum Temperature Recorded = ", min_temp, "°F")

# How do density and snow depth vary over time? Create a single plot showing both snow depth and density as a function of time. 
#Note you can use the lines() or points() functions to add more to an existing scatter/line plot. 
plot(snotel$date,snotel$depth)

lines(snotel$date,snotel$density,col="red")

# Setting the larger right margin
#First value is the bottom line
#second value is the left line
#third value is the top line
#fourth value is the right line
par(mar = c(5, 5, 5, 5))

plot(snotel$date,
     snotel$depth,
     col = "blue",
     xlab = "Date",
     ylab = "Depth (inches)",
     ylim = c(min(snotel$depth), max(snotel$depth)),
     type = "l")

title(main = "Depth and Density Over Time")
axis(2, col.axis = "blue")

par(new = TRUE)

plot(snotel$date,
     snotel$density,
     col = "red",
     xlab = "",
     ylab = "",axes = FALSE,
     type = "l")

axis(4, col.axis = "red")
mtext("Density (percentage)", side = 4, line = 2.5, col = "red",)



#What day seems to mark the transition from clear to stormy weather?
#looking at the graph, jan 4th seems to be the day

#Is there a correlation between snow water equivalent and precipitation accumulation? Describe the strength and direction of any relationship.
plot(snotel$swe,
     snotel$prec_accum)

model <- lm(snotel$swe ~ snotel$prec_accum)

strength <- round(as.numeric(model$coefficients[2]),3)

paste0("Positive correlation of ", strength, " when comparing swe to precipitation accumulation")

# Create a new column called swe_perc showing the snow water equivalent as a percent of the median historical swe value for each day. Plot these values and describe how the swe values track with the median historical values?

snotel$swe_perc <- snotel$swe/snotel$swe_med * 100
plot(snotel$date,snotel$swe_perc)

#   How much snow (depth) was gained or lost from the start to end of time period? How much snow water equivalent was gained or lost?

max_day_df <- snotel[(snotel$date == max(snotel$date)),]
min_day_df <- snotel[(snotel$date == min(snotel$date)),]

max_day_df$depth - min_day_df$depth
max_day_df$swe - min_day_df$swe

#   Write a script to calculate the daily incremental change in both snow depth and SWE over the 30 day period. Sum both the gains and losses from each and compare these values to the values in the previous question. What might account for the differences?
differences = NULL

for (i in 1:(length(snotel$depth) - 1)) {
  diff = snotel$depth[i+1]-snotel$depth[i]
  print(diff)
  differences <- c(differences, diff)
}

sum(differences)


differences2 = NULL

for (i in 1:(length(snotel$swe) - 1)) {
  diff2 = snotel$swe[i+1]-snotel$swe[i]
  print(diff2)
  differences2 <- c(differences2, diff2)
}

sum(differences2)


#   Is there a relationship between air temperature and snow depth? Run a linear model, plot the results (showing scatterplot and linear model), and report the R2 and p-value. Run a correlation test as well.
plot(snotel$t_ave,snotel$depth)
lm(snotel$t_ave~snotel$depth)

# Create a new DataFrame that is a subset of this data and only include dates after the storms arrived (i.e. keep days after new snow started accumulating). Run the same tests as in the previous question and compare the statistical strength. How has the relationship changed at all?
sub_snotel = snotel[snotel$date >= as.Date("2024/1/04"),]

plot(sub_snotel$t_ave,sub_snotel$depth)
lm(sub_snotel$t_ave~sub_snotel$depth)

# Assess the relationship between snow density and temperature, as well as any other variables you think might relate to one another. Describe any patterns or relationships you notice in your initial analysis of this dataset. Advanced students should try multiple linear regression.
library(ggplot2)

linear_model <- (lm(snotel$t_ave~snotel$density))
plot(snotel$t_ave,snotel$density)
abline(linear_model,col = "green")

summary(linear_model)

