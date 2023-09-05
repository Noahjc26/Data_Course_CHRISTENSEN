
# Assignment 2

#root "/"
# the current directory "."
#The directory above the current one ".."


list.files(.)
list.files(path="Data")
csv_files <- list.files(path="Data",pattern = ".csv") 
# option minus for <- 
length(csv_files)

?length # the question mark goes to the 'Help' tab

df <- read.csv("./Data/wingspan_vs_mass.csv") 
# df = data frame

head(df,n=5)  #head shows first 6 lines, n specifies the number of lines)
list.files(path="Data",
           recursive = TRUE,
           pattern = "^b")


list.files(path="Data",
           recursive = TRUE,
           pattern = ".csv")

