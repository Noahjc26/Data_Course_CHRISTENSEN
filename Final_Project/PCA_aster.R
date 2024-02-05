library(terra)
library(raster)
library(RStoolbox)

#reading in aster a sa stack
aster <- stack("../../ASTER/2005_10_01/full_cleaned.tif")


#looking at aster imagery
plotRGB(aster,4,3,2,stretch = "hist")

#performing pca on aster
pca1 <- rasterPCA(aster)

# View summary of PCA results
summary(pca1)

par(mar = c(5,5,5,5))
#looking at variance
plot(pca1$model)

#plotting the first four pca's
plot(pca1$map[[1:4]])

plot(pca1$map[[4]])

#making into stacked raster to plot rgb
raster_pca <- (pca1$map)
raster_pca <- stack(raster_pca)

#looking at alteration zones
plotRGB(raster_pca,1,2,3,stretch="hist")


unsup_pca <- unsuperClass(aster,nClasses = 10)

str(unsup_pca)
plot(unsup_pca$map)

?unsuperClass



#support vector machine
#plotting pca vs other plots
