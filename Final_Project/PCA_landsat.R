library(terra)
library(raster)
library(RStoolbox)

#reading in aster a sa stack
landsat <- stack("../../landsat/LC09_L2SP_038033_20231019_20231020_02_T1/cropped.tif")


#looking at aster imagery
plotRGB(landsat,4,3,2,stretch = "lin")

#performing pca on aster
pca1 <- rasterPCA(landsat)

# View summary of PCA results
summary(pca1)

par(mar = c(5,5,5,5))
#looking at variance
plot(pca1$model)

#plotting the first four pca's
plot(pca1$map[[1:3]])

#making into stacked raster to plot rgb
raster_pca <- (pca1$map)
raster_pca <- stack(raster_pca)

#looking at alteration zones
plotRGB(raster_pca,1,2,3,stretch="hist")



