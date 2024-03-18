#for loops
#if else
#indexing
#boolean expression
#dealing with NA values
#base plotting
#ggplot
#dplyr
#Date
#order/sort/match/which/which.max/which.min
#function


vect = c(1,4,7,NA,6,6)

vect[!is.na(vect)]




library(terra)

#vect data
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5, -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9, 36.2, 39, 41.6, 36.9)
lonlat <- cbind(longitude, latitude)

#vect function in terra
pts <- vect(lonlat)
class(pts)
plot(pts)

#assigning crs
crdref <- "+proj=longlat +datum=WGS84"
pts <- vect(lonlat,crs=crdref)
# or like this
crs(pts) <- crdref
#or even like this
pts <- vect(lonlat,crs="EPSG:4326")

plot(pts)


#fake data
precipvals <- runif(nrow(lonlat),min=0,max=100)
precipvals
df <- data.frame(ID=1:nrow(lonlat),precipvals)
ptv <- vect(lonlat,atts=df,crs="EPSG:4326")
ptv
plot(ptv,cex = 1+ precipvals/100,"precipvals")

#making a line
l <- vect(lonlat,type="lines",crs="EPSG:4326")
lines(l)

#terra preset dataset
f <- system.file("ex/lux.shp",package="terra")


plot(vect(f),"NAME_1")


plot(vect("./Utah_ZIP_Code_Areas/ZipCodes.shp"),"NAME")

p <- (vect("./Utah_ZIP_Code_Areas/ZipCodes.shp"))

plot(p[,"NAME"])



#plotting parameters
par()$mar

par(mfrow=c(1,2))
plot(p)
plot(p)

?par
par(mfrow=c(1,1))

z <- rast(p)
dim(z) <- c(2,2)
values(z) <- 1:4
names(z) <- "Zone"
z <- as.polygons(z)
plot(z, add=T, border = "blue", lwd=5)
lines(z,col="red",lwd=3)

z2 <- z[2,]

plot(z2, add=T, col = "yellow", lwd=5)
text(z, "Zone")


plot(p,  "NAME")

#dissolve
plot(aggregate(p,"COUNTYNBR"))

#erase
e <- erase(p,z2)
plot(e)


#clipping
c <- crop(p,z2)
plot(c)
