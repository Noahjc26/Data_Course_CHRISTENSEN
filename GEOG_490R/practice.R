library(terra)

vector <- vect("./Land_Ownership/Land_Ownership.shp")
unique(vector$natl_lgd)
plot(vector, "natl_lgd", main = "Land Ownership over Utah")


plot(extent,col=NA,lwd=2,add=T,border="red")




trails <- vect("./Land_Ownership/trails/TrailsAndPathways.shp")

extent <- ext(-112,-111,37,38)

plot(crop(vector,extent),"natl_lgd")

plot(crop(trails,extent),add=T,col="white",lwd=2)

grades <- sample(50:100,30,replace = T)

letter_grade = NULL

for (i in 1:length(grades)){
  if (grades[i] >= 90){
    letter_grade[i] = "A"
  } else if (grades[i] >= 85){
    letter_grade[i] = "B"
  } else if (grades[i] >= 80){
    letter_grade[i] = "C"
  } else if (grades[i] >= 75){
    letter_grade[i] = "D"
  } else {
    letter_grade[i] = "E"r
  } 
}

df = data.frame(grades,letter_grade)
df
