
# 1. Data types and structures

#Using the start (Jan, 8) and end (April, 23) dates of the spring 2024 semester, answer questions 1-3:
  
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


#Use regular expression and the following two vectors to respond to questions 4-5:
  
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

# phone numbers
phones <- c("(525) 692-8788", "(748) 654-2046", "(865) 970-8066", "(310) 472-4003", 
            "(689) 943-4206", "920.680.4936", "323.171.5088", "265.687.3906", 
            "316.674.1293", "389.240.9468", "562 322 3503", "140 627 7741", 
            "530 215 9943", "189 705 9565", "415 873 7128", "133-854-8126", 
            "615-508-4357", "112-407-4979", "168-377-4229", "994-188-6602",
            "123-456-789", "abc-123-4567", "123 45 6789", "1a2b3c4d5e", 
            "12345", "67890", "1234", "6789", "12345678903", "12345678901")

# With the emails vector, write a function to identify only valid email addresses containing exactly one @ symbol and at least one period using regular expression. (can use grep() or another)

?grep
length(emails)
grep("@",emails)
grep(".",emails)


valid_emails <- emails[]

# Using the phones vector, write a regular expression pattern to identify all valid 10-digit phone numbers in the phones vector, regardless of how they are formatted with parentheses, spaces, dots, etc. (Tip: consider the format of different 10-digit numbers and account for optional non-digit characters, 3 digits, optional non-digits, 3 digits, optional non-digits, and 4 digits) - advanced students should write a script to check to try and double check that their responses are valid.


# Subset all valid phone numbers that contain the format XXX-XXX-XXXX and replace the “-” with “_” using gsub or other function.
# 
# 2. Snotel data
# 
# Read in csv file provided in canvas. This file contains daily snow and meteorological data for 30 days from December, 24, 2023 to January, 22 2024 collected at the snow telemetry station at the Timpanogos Divide.
# 
# After reading in the data, convert dates column to a date format and . Precipitation and Snow Water Equivalent units are in inches.
# 
# For context, snow water equivalent is the amount of water in the snowpack or a volume of snow and is calculated by multiplying the snow density (ρ) by depth (d):
#   
#   SWE=ρ∗d
# a brief explanation of other variables are also listed:
#   
#   date - Date string corresponding to local time
# swe - Snow water equivalent at the start of the day (inches)
# swe_med - Historical median swe at the start of the day for 1991-2020 (inches)
# depth - Snow depth (inches)
# prec_accum - Total accumulated (cumulative) water for current water year (Oct 1st) (inches)
# prec_med - historical median prec_accum for 1991-2020 (inches)
# t_max - Air temperature maximum of the day (oF)
# t_min - Air temperature minimum of the day (oF)
# t_ave - Air temperature average of the day (oF)
# Respond to the following questions:
#   
#   Use the equation above to estimate the density of the snowpack each day, add this as a new column to your DataFrame. List the average and standard deviation of the following: snow density (in percent), snow depth, and average temperature. Also list the snow warmest and coldest temperature recorded during the time period. Concatenate this information into a single print or cat statement.
# 
# How do density and snow depth vary over time? Create a single plot showing both snow depth and density as a function of time. Note you can use the lines() or points() functions to add more to an existing scatter/line plot. What day seems to mark the transition from clear to stormy weather?
#   
#   Is there a correlation between snow water equivalent and precipitation accumulation? Describe the strength and direction of any relationship.
# 
# Create a new column called swe_perc showing the snow water equivalent as a percent of the median historical swe value for each day. Plot these values and describe how the swe values track with the median historical values?
#   
#   How much snow (depth) was gained or lost from the start to end of time period? How much snow water equivalent was gained or lost?
#   
#   Write a script to calculate the daily incremental change in both snow depth and SWE over the 30 day period. Sum both the gains and losses from each and compare these values to the values in the previous question. What might account for the differences?
#   
#   Is there a relationship between air temperature and snow depth? Run a linear model, plot the results (showing scatterplot and linear model), and report the R2 and p-value. Run a correlation test as well.
# 
# Create a new DataFrame that is a subset of this data and only include dates after the storms arrived (i.e. keep days after new snow started accumulating). Run the same tests as in the previous question and compare the statistical strength. How has the relationship changed at all?
#   
#   Assess the relationship between snow density and temperature, as well as any other variables you think might relate to one another. Describe any patterns or relationships you notice in your initial analysis of this dataset. Advanced students should try multiple linear regression.
# 
# 
# 
# Submitting
# 
# Upload either a single document showing code, responses, and plots… or a separate R script with a word doc or pdf containing all plots and responses. Be sure to support your responses with stand-alone code. Comment out any text and annotate each step well.
# 
# Please reach out if you have any questions or concerns!
#   
#   This assignment is due in one week but may be turned for an additional week with a late penalty of -5%