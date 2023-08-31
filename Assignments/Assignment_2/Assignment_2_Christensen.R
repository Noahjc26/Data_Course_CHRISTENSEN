
# Assignment 2

#notes absolute vs relative ./..
list.files(.)
list.files(path="Data")
csv_files <- list.files(path="Data",pattern = ".csv") 
# option minus for <- 
length(csv_files)
?length

df <- read.csv("./Data/wingspan_vs_mass.csv") 
# df = data frame
head(df,n=5)
list.files(path="Data",
           recursive = TRUE,
           pattern = "^b")
