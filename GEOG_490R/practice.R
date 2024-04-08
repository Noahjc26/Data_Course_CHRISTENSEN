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
    letter_grade[i] = "E"
  } 
}

df = data.frame(grades,letter_grade)




# Rasters
x <- rast()


x <- rast(ncol =100,nrow = 100,xmin = -1000,xmax = 1000, ymin = -500, ymax = 900, crs = "+proj=utm +zone=48 +datum=WGS84")
values(x) = 1:ncell(x)
plot(x)

x[] <- runif(ncell(x))
plot(x)

x[]


newcrs <- "+proj=robin +datum=WGS84"

pr1 <- project(x,newcrs)

plot(pr1)



