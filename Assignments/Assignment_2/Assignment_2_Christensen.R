
# Assignment 2

#root "/"
# the current directory "."
#The directory above the current one ".."

list.files(path="Data")

csv_files <- list.files(path="Data",
                        full.names = TRUE,
                        pattern = ".csv") # option minus for <- 
length(csv_files)

?length     # the question mark goes to the 'Help' tab

df <- read.csv("./Data/wingspan_vs_mass.csv")  # df = data frame

head(df,n=5)    #head shows first 6 lines, n specifies the number of lines)

bfiles <- list.files(path="Data",
           recursive = TRUE,
           full.names=TRUE,
           pattern = "^b")

bfiles

for(i in bfiles) {
  print(readLines(i,n=1))
} #i is just "iteration" 
  
for(i in csv_files) {
  print(readLines(i,n=1))
}

#"Command-Option-B" runs all lines above