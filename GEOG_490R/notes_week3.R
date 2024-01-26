cat("Hi","\n",134)


full <- sample(1:100,25)

#making if else statements to subset numbers between 0-25, 25-50, and 50-100
for (i in full) {
  if (i < 26) {
  print(paste("g1",i))
  } 
  else if (i < 51) {
  print(paste("g2",i))
  }
  else{
  print(paste("g3",i))
  }
}



class_df <- data.frame(
  Name = c( "Matt", "Noah", "Andrew", "Abi", "Brian", "Emmaline", "Anna", "Carter", "Spencer", "Emily"),
  age = c(27,22,24,24,23,21,24,20,23,40),
  eye_color = c("Blue","Blue","Blue","Brown","Brown","Blue","Blue","Green","Brown","Hazel"),
  hair = c( "Brown", "Blond", "Brown", "Brown", "Brown", "Brown", "Blond", "Brown", "Blond", "Brown"),
  Height_ft = c(5, 5, 6, 5, 6, 5, 5, 5, 6, 5),
  height_in = c(9, 7, 0, 5, 0, 11, 5.5, 10, 1, 6),
  num_sibs = c(2, 3, 4, 4, 3, 7, 3, 3, 3, 1),
  Major = c("Geography","Geology","Geography","Geology","ENVT","Geology","ENVT","Geography","Geography","GIS"),
  d_o_b = c("03/24/96", "05/21/01", "05/12/99", "03/29/99", "07/07/00", "11/06/02", "12/14/99", "02/15/03", "12/14/00", "1/28/83")
)

class_df

#plot with two of these
plot(class_df$num_sibs,class_df$age)
hist(class_df$age)

boxplot(class_df$age)
abline(h = mean(class_df$age),col='red' )

dim(class_df)

class_df$total_height = class_df$Height_ft + (class_df$height_in/12)

class_df$total_height

cor.test(class_df$age,class_df$total_height)


summary(lm(class_df$age~class_df$total_height))

#changing data from character to actual data variable
class_df$d_o_b <- as.Date(class_df$d_o_b,tryFormats = "%m/%d/%y")

plot(class_df$total_height~class_df$d_o_b)

seq(min(class_df$d_o_b),max(class_df$d_o_b),by = "day")


names(class_df)
class_df[1:5,c("hair","height","Name")]

class_df[class_df$total_height > 5.8,]


getwd()
list.files(".")
