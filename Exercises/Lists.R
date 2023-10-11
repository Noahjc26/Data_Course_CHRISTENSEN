library(tidyverse)
library(purrr)

#lists
a <- 1:10
b <- letters
c <- c(TRUE,TRUE,FALSE)

x <- list(a,b,c)
x[[1]] #double square bracket gives you element of the list
x[[1]][3] #single square bracket give element inside element of list

x[1]

for (i in 1:3) { #only looking at first element in each item
  print(x[[i]][1])
  
}

map(x,1) #also looking at first element in each item, remains as a list
map_chr(x,1) #turns it into a character vector


y <- list(a=iris, #creating a list of dataframes
     b=mtcars)
y
map(y,class)
map(x,class)



#function that takes 1st and 2nd columns and multiplies them and makes new col named "products"

make_products <- 
function(x){
  if(!is.numeric(x[,1])){
   # stop("Hey idiot, that first column is not numeric. Try again.")
    x[,1] <- as.numeric(x[,1])
    
  }
  newcol <- x[,1] * x[,2]
  x["products"] <- newcol
  return(x)
}
# lapply(iris,as.character)

map(y,make_products)
y$a$Sepal.Length <- as.character(y$a$Sepal.Length) #digging into list through dollar signs then changing to character

