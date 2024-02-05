library(rvest)
library(janitor)
library(tidyverse)

# 1
a = 3
b = 6

c<- sqrt(a^2 + b^2)

c > b
c > a

# 2
# sequence from 10-25 by 3
y <- seq(10,25,3)
x = (y-5)/4

plot(x,y)

#determining if the slope is equal to 4
4 == (y[6]-y[1])/(x[6]-x[1])

# 3
# Reading in the table from Wikipedia
page = read_html("https://en.wikipedia.org/wiki/List_of_national_parks_of_the_United_States")

# Obtain the piece of the web page that corresponds to the "wikitable" node
table = html_node(page, ".wikitable")

# Convert the html table element into a data frame
table = html_table(table, fill = TRUE)

#cleaning names
table <- table %>% 
  clean_names()

#removing words and km from area column values
table_2 <- table %>% 
  separate(col = area_2023_8,
           sep = " ",
           into = "area_acres")

# turning area_acres column into numeric
table_2$area_acres <- as.numeric(gsub(",", "", table_2$area_acres))

# turning visitors column into numeric
table_2$recreation_visitors_2022_11 <- as.numeric(gsub(",", "", table_2$recreation_visitors_2022_11))

table_2 %>% 
ggplot(aes(x=date_established_as_park_12,y=recreation_visitors_2022_11, fill = area_acres)) +
  geom_point()



# 4
#showing the average acres in the area
paste(mean(table_2$area_acres), "acres")

#finding total area of all parks combined
paste(sum(table_2$area_acres[1:length(table_2$area_acres)]),"total acres")

# 5
# park with most visitors
table_2$name[(which.max(table_2$recreation_visitors_2022_11))]

#park with least visitors
table_2$name[which.min(table_2$recreation_visitors_2022_11)]

# difference in visitors in percentage
max(table_2$recreation_visitors_2022_11)/min(table_2$recreation_visitors_2022_11) * 100

#total number of visitors
sum(table_2$recreation_visitors_2022_11[1:length(table_2$recreation_visitors_2022_11)])

# 6
#histogram showing national parks by area
hist(table_2$area_acres)

#using boolean expression to show how many parks are larger than the median park area
median_area <- median(table_2$area_acres)

# applying boolean expression
table_2$area_acres > median_area

# Count TRUE and FALSE values
table(table_2$area_acres > median_area)

# 7

# Split the date column into components
date_components <- strsplit(table_2$date_established_as_park_12, "[ ,]+")

# Extract month, day, and year
month <- sapply(date_components, function(x) x[1])
day <- sapply(date_components, function(x) as.numeric(x[2]))

# Extract numeric part of the year and handle cases with additional characters
year <- sapply(date_components, function(x) as.numeric(sub("\\[.*\\]$", "", x[3])))

# Create new columns in the data frame
table_2$establishment_month <- month
table_2$establishment_day <- day
table_2$establishment_year <- year

# Print the modified data frame
print(table_2)

#determining rate at which national parks were created
(max(table_2$establishment_year) - min(table_2$establishment_year)) / length(unique((table_2$name)))
  
# around 2.35 years for every new park to be established. This estimate does not take into account the youngest or oldest month so it could be off by 11 months in the max and the min

# 8
#plot showing year established versus size of the park
plot(table_2$establishment_year,table_2$area_acres)
#It seems like the size of these parks has been increasing as the years have progressed. However, the most recent 20 years were fairly small parks.

# plot showing park size vs visitation
plot(table_2$area_acres,table_2$recreation_visitors_2022_11)
# This plot shows an almost asymptotically negative trend from most visitors going to smaller area parks, and less visitors at larger parks.

# 9
summary(table_2)

sapply(table_2, class)

# 10 

#looking at the structure, classes, and summary of iris
sapply(iris,class)
summary(iris)
str(iris)
length(iris)
names(iris)

# 11

#creating TRUE/FALSE based on greater than or equal to 7 and less than or equal to 5
boolean_values <- iris$Sepal.Length >= 7 & iris$Petal.Length <= 5

# Subset the iris data frame based on boolean values
selected_rows <- iris[boolean_values,]

# Print the species of the selected rows
print((selected_rows$Species))

# creating TRUE/FALSE based on greater than 
boolean_values <- iris$Sepal.Length >= 7 & iris$Petal.Length <= 5.8 

# Subset the iris data frame based on boolean values
selected_rows <- iris[boolean_values, ]

# Print the species of the selected rows
print((selected_rows$Species))

# 12

# Define colors for each species
species_colors <- c("red","green", "blue")

# Plot all points with specified colors for each species
plot(iris$Petal.Width, iris$Petal.Length,
     xlab = "Petal Width (cm)",
     ylab = "Petal Length (cm)",
     main = "Iris Flower Petal Dimensions",
     col = species_colors[(iris$Species)])

# Add legend
legend("topleft",
       legend = unique(iris$Species),
       col = c("red", "green", "blue"),
       pch = 1,
       title = "Species")

# Add a regression line
abline(lm(Petal.Length ~ Petal.Width, data = iris), col = "black")

#There is a positive slope comparing petal length vs petal width. It is split into three clumps based on species but has an overall positive trend.
#you could use summary statistics and the lm equation to determine more analytical results.










