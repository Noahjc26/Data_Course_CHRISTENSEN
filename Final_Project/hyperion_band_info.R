# reading in csv of hyperion band information
# removing random extra columns
# removing random extra rows
Hyperion_Bands <- read.csv("Hyperion_Bands_Wavelengths.csv") %>%
  select(-c(X,X.1,X.2,X.3))
Hyperion_Bands <- Hyperion_Bands[-(199:235),]

#bremoving nm and FWHM: in wavelength column
Hyperion_Bands$Wavelength <- gsub("nm","",Hyperion_Bands$Wavelength)
Hyperion_Bands$Wavelength <- gsub("FWHM:","",Hyperion_Bands$Wavelength)

# separating Wavelength and FWHM into their own columns
# separating VNIR and SWIR from band names
Hyperion_Bands <- Hyperion_Bands %>% 
  as.data.frame() %>% 
  separate(Wavelength,into = c("Wavelength_nm","FWHM_nm"),sep=",",convert = TRUE) %>%
  separate(Description,into = c("Description", "Temp", "Temp2"),sep=" ") %>%
  rename("Bands" = "Band_Name")

# removing temp columns 
Hyperion_Bands <- subset(Hyperion_Bands, select = -c(Temp, Temp2))

#adding value in new column based on Hyperion_Bands$Description
vect <- if_else(Hyperion_Bands$Description == "VNIR",40,80)

#adding vect as new column
Hyperion_Bands$Rad_Conv = vect

#adding columns with lower and upper wavelength values
Hyperion_Bands <- Hyperion_Bands %>%
  mutate(lower = Wavelength_nm - FWHM_nm) %>% 
  mutate(upper = Wavelength_nm + FWHM_nm)

#saving as RDS
saveRDS(object = Hyperion_Bands,file = "./cleaned_hyperion_band_info.rds")
