#pca

library(terra)
library(raster)
library(RStoolbox)
aster <- stack("../../ASTER/2005_10_01/full_cleaned.tif")

plotRGB(aster,4,3,2,stretch = "hist")


pca1 <- rasterPCA(aster)
pca2 <- rasterPCA(aster, nSamples = 5000)  # sample 5000 random grid cells
pca3 <- rasterPCA(aster, norm = FALSE)  # without normalization

plot(pca1)

# View summary of PCA results
summary(pca1)


plotRGB(pca1$map)
raster_pca <- (pca1$map)
raster_pca <- stack(raster_pca)

#looking at alteration zones
plotRGB(raster_pca,2,3,4,stretch="hist")

plot(raster_pca[[1]])

