# Install and load the hyperSpec package
# install.packages("hyperSpec")
library(hyperSpec)

# Create a sample dataframe
df <- data.frame(
  Wavelength = seq(400, 2500, by = 10),
  Sample1 = rnorm(211),
  Sample2 = rnorm(211),
  Sample3 = rnorm(211)
)

# Assuming your dataframe has the wavelength in the first column and reflectance values in the remaining columns

# Extract numeric data from the dataframe
data_matrix <- as.matrix(t(df[, -1]))

# Create a hyperSpec object
hyperspec_object <- new("hyperSpec", wavelength = df$Wavelength, spc = data_matrix)

# Plot the original spectra
plot(hyperspec_object)
