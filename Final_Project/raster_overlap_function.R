library(raster)
library(sp)
library(terra)
library(rgdal)
library(caTools)

AVIRIS <- rast("../../AVIRIS/overlap/f190821t01p00r12rdn_e/f190821t01p00r12rdn_e_ort_glt")

AVIRIS <- rectify(AVIRIS)
plot(AVIRIS)
AVIRIS
AVIRIS <- read.ENVI("../../AVIRIS/CC_Flights/f100825t01p00r10/f100825t01p00r10rdn_b/f100825t01p00r10rdn_b_ort_igm")




terra::rectify(AVIRIS)
plot(e)
plot(e1)
plot(HYPERION)
plot(AVIRIS)
ext(AVIRIS)
ext(HYPERION)
plot(AVIRIS)
HYPERION <- raster("../../Hyperion/overlap/EO1H0380342012191110KF_1T/EO1H0380342012191110KF_B001_L1T.TIF")
e <- extent( 452262.1, 457516.7, 4133367, 4199334 )
e1 <- extent(327600, 358800.000326157, 4101899.99942398, 4199400)
crop(HYPERION,e)
test_intersection <- function(a,b){
  #reads in two rasters and tests for overlap T or F
  # if returns TRUE then there is overlap
  if(class(a)!='Extent'){ a  = extent(a)}
  if(class(b)!='Extent'){ a  = extent(b)}
  !class(try(intersect(a,b),T ))=='try-error'}

interc


test_intersection <- function(a,b){
  #reads in two rasters and tests for overlap T or F
  # if returns TRUE then there is overlap
  # try error is included b/c errors has come up with other test data
  !(class(try(intersect(a,b),T ))=='try-error' | is.null(intersect(a,b)))
}

# Test function
intersect(AVIRIS,HYPERION)
test_intersection(AVIRIS,HYPERION)

intersect(e1,e3)
test_intersection(e1,e3)


test_intersection(e1,e2)

intersect(e1,e3)
test_intersection(e1,e3)
