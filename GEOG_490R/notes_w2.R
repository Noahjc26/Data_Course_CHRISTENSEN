# Data types
num = 140
ch = "12"
class(num)

is.numeric(ch)
is.character(ch)

as.numeric(ch)

TRUE & FALSE

(99 > 15) | (55 == 54.99999)

seq(10,100,length.out = 55)


matrix(1:9, nrow = 3, ncol = 3)

dim(mtcars)
mtcars <- mtcars
summary(mtcars)


matrix()



#####-----------

# I stand for iteration
for (i in seq(1:100)) {
  
}

for (variable in vector) {
  paste
}




s_vect <- sample(1:100,25)

new_vect <- c(0)

# adding all values above 50 to new_vect
for (i in (s_vect)) {
  if (i > 50) {
    new_vect <- c(new_vect,i)
  }
  vect_mean <- mean(new_vect)
  phrase <- paste0("Mean after iteration ",i,": ",vect_mean)
  print(phrase)
}




#plotting
mean(mtcars$qsec)

plot(mtcars$hp,mtcars$mpg,
     main = "Scatter plot of Horsepower vs Milage",
     xlab = "Miles/Gallon",
     ylab = "Horse Power",
     pch = 12,
  )

# Adding a regression line with a y intercept of 35 and a slope of -0.1
abline(a= 35, b=-0.1)

# Adding a dotted vertical line at 200
abline(v = 200, lty = 2)

#Histogram function
hist(mtcars$mpg, breaks = 10)

#Boxplots
boxplot(mpg~cyl,data = mtcars)
s