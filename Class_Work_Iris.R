iris
iris[1,] #variables are columns, observations are rows
View(iris)
summary(iris$Petal.Width)
names(iris)
iris$Petal.Length*iris$Petal.Width

iris$Petal.Area <- iris$Petal.Length*iris$Petal.Width

for(i in names(iris)){
  x <- iris[,i]
  print(summary(x))
}

x <- c("sucks","is cool")
for(whatever in x){
  print(paste0("Your mom ",whatever))
}
