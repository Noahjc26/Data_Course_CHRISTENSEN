aviris <- rast("/Users/noah/Desktop/AVIRIS/overlap/f190821t01p00r12rdn_e/f190821t01p00r12rdn_e_sc01_ort_img")
plot(aviris[[40]])
plotRGB(50,20,10)

aviris2 <- rectify(aviris)
plot(values(aviris[[1]]))
