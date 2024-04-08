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

v <- vect(f)

r <- rast(system.file("ex/elev.tif",package = "terra"))

plot(r,breaks = 3, type = "interval",breakby = "eqint")
plot(v,add=T)
text(v,"NAME_1",halo = T, add = T)


#######################
#######################
#######################
#######################
#######################
#statistics to look at
hist(r,breaks=120)
global(r, na.rm = T)
#zonal statistics
zonal(r, z=v, na.rm = T)

plot(v,"mean")
v$mean <- round(zonal(r, z=v, na.rm = T),2)

#######################
#######################
#######################
#######################
#######################
#terrain parameters
slope <- terrain(r, v = "slope", unit = "degrees")
aspect <- terrain(r, v = "aspect", unit = "degrees")
plot(slope, col = grey.colors(100))
plot(r,col = terrain.colors(25), add = T,alpha = 0.5)

plot(shade(slope*pi/180, aspect*pi/180, angle = 40, direction = 270), col = grey(0:100/100))

m <- c(0, 1000, 1,
       1000, 2000, 2)

rcl_mat <- matrix(m, ncol = 3, byrow = T)

r_class <- classify(r*3.28,rcl_mat)
plot(r_class)



p_sub <- v[v$NAME_2 == "Diekirch",]

r_crop <- crop(x=r, y=p_sub,mask=T, touches = F)


plot(r_crop)

max_value <- max(na.omit(values(r)))
interval <- (max_value - min_value) / 3

break_1 <- max_value - interval
break_2 <- max_value - interval *2
break_3 <- min_value


r$elev_class[r$elevation < break_1] = 5


r <- vect(f)
names(r)

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
