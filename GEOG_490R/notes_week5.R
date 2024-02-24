
#create random sample
x <- sample(1:100,50)

#set x_mean as the mean of x
x_mean <- mean(x)

#make true/false vector for each x if it is above x_mean
boolean <- x > x_mean

#getting index of all true statements
statement_t <- which(boolean)

#making new vector that is all values above 50
x[statement_t]

x2 <- sample(1:100,100)

x2[x2>=10 & x2<=20]

x2>=10 & x2<=20




dice_roll <- function(n){
  return((sample(1:6, n, replace=TRUE)))
}

hist(dice_roll(100000))


dnd <- function(n,dice,mod){
  roll = ((sample(1:dice, n, replace=TRUE)))
  final = sum(roll) + mod
  return(final)
}

dnd(2,20,5)


# create a vector, subset every other variable
vect <- 1:100

x <- seq(1,length(vect),2)

vect[x]

#looking at numbers greater than 20 but less than 50
vect[vect >= 20 & vect <= 50]



