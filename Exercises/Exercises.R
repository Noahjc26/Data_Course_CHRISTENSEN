library(tidyverse)
library(ggforce)
library(patchwork)
library(ggpubr)
library(gganimate)
library(GGally)
library(plotly)
library(kableExtra)
library(gganimate)
library(readxl)
library(janitor)
library(lubridate)
library(skimr)

#l
#### Vector ####
#Exercise 1
x <- c(4,6,5,7,10,9,4,15)
x<7

#Exercise 2
p <- c(3,5,6,8)
q <- c(3,3,3)
p+q

#Exercise 3
a=c(1,3,4,7,10,0)
b=c(1,2)
a+b

#Exercise 4
z <- 0:9
digits <- as.character(z)
as.integer(digits)

#Exercise 5
x <- c(1,2,3,4)
(x+2)[(!is.na(x)) & x>0] ->k
k
?is.na

#Exercise 6
s=c("a","b","c","d","e")
t=c("f","g","h","i","j")
s+3
c(s,t)

#Exercise 7
s=c("a","b","c","d","e")
v=1:5
z=c(s,v)
z[5:10]


#### Factors ####
library(tidyverse)
library(gapminder)
data("gapminder")
df = gapminder
head(gapminder)

#Exercise 1
levels(df$continent)
levels(df$continent) <- c(levels(df$continent), "Antarctica")
levels(df$continent)

#Exercise 2
levels(df$continent) <- c(levels(df$continent),
                          "North America",
                          "South America",
                          "Central America")
levels(df$continent)

#Exercise 3
?levels
(df$country=="Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador",
   "Paraguay","Peru","Uruguay","Venezuela")
?arrange()
?relocate()
 ?
#### Characters ####
library(tidyverse)

#Exercise 1
vector = "Good morning! "
nchar(vector)

#Exercise 2
x <- c("Open", "Sesame ")
y <- c("You","Suck.")
nchar(x)
nchar(c(x,y))

#Exercise 3
m <- "The captial of the United States is Washington, D.C."

unlist(str_split(m," "))

str_trunc(m,11, ellipsis = "")

str_sub(m,start=13,end=25)

str_sub(m,start=37,end=46)

#Exercise 4
paste(m,", you idiot!", sep = "")

paste(str_sub(m,start=20,end=32),", you idiot!", sep = "")

#Exercise 5
q= "What is the capital of the United States?"
c(q, paste0(m,", you idiot!")) 

d = str_split(c(q, paste0(m,", you idiot!")), pattern = " ")

#Exercise 6
c(unlist(map(d,1)),"Heck!?")

unlist(map(d,2))
map(d,1)

#Exercise 7
t = c("a","ab","c","d","e","fa")
grep("a",t)
grepl("a",t)
f = c("b","ca","at","c","e","aa")
v = list(f,t)
v
grep("a",v)
grepl("a",v)
grep("What",d)
grepl("What",d)
d

#Exercise 8
q
str_replace(q,"a","A")
str_replace_all(q," ","_")



#### Regular Sequences ####
#Exercise 1
seq(1,10,by=2)
seq(1,10,by=3)

#Exercise 2
seq(9,45,by=9)

#Exercise 3
seq(1,10,length.out = 5)
seq(1,10,length.out = 3)

#Exercise 4
x=1:5
rep(x,2)
rep(x,2,each=2)
rep(x,each=4)

#Exercise 5
x = "Hip"
y = "Hooray"
rep(c(rep(x,2),y),3)

#Exercise 6
seq(100,50,by=-5)

#Exercise 7
Semester_Start = as.Date("2019-08-19")
Semester_End = as.Date("2019-12-05")
seq(Semester_Start,Semester_End,by = "week")
midterm = seq(Semester_Start,Semester_End,length.out = 3)[2]
midterm


#### Indexing ####
#Exercise 1
x = c("ss","aa","ff","kk","bb")
x[1]
x[c(1,3)]

#Exercise 2
d = data.frame(Name = c("Betty","Bob","Susan"),
               Age = seq(20,30,length.out = 3),
               Height_cm = c(490,22,0))
d
d[c("Name","Age")]
d[c("Age","Name","Height_cm")][1,]

#Exercise 3
d$Name
d$Age[2]

#Exercise 4
d$Age > 20
d[d$Age > 20,]

d[d$Height_cm < 100,]
d[1,c("Name","Age")]

#### Missing Values ####
#Exercise 1
X = c(NA,3,14,NA,33,17,NA,41)
!is.na(X)

X[!is.na(X)]

#Exercise 2
Y = 21:28
Z = data.frame(X,Y)
Z
Z[is.na(Z)] <- 0
Z

P = c(X,33,NA,400,12,0,15)
P
P[is.na(P)] <- 10
P

#Exercise 3
W <- c(11,3,5,NA,6)
is.na(W)

#Exercise 4
A <- c(33,21,12,NA,7,8)
mean(A[!is.na(A)])

#Exercise 5
data("Orange")
head(Orange)

Orange[Orange==118] <- NA
head(Orange)

#Exercise 6
c1 <- c(1,2,3,NA)
c2 <- c(2,4,6,89)
c3 <- c(45,NA,66,101)
X <- data.frame(c1,c2,c3)
X
complete.cases(X)

X[rowSums(is.na(X))>0,]

#Exercise 7

df <- data.frame (Name = c("NA", "Joseph", "Martin", NA,"Andrea"),
                  Sales = c(15,18,21,56,60),
                  Price = c(34,52,21,44,20),
                  stringsAsFactors = FALSE)
df[df=='NA'] <- NA
df[!is.na(df$Name),]

#### Loops ####
for(i in 1:3){
  x <- paste0("Number ",i)
  print(x)
}

planets <- c("Mercury","Venus","Earth","Mars","Jupiter","Saturn","Uranus","Neptune")
n <- 1
newvector <- c()
for (i in planets) {
  newvector[n] <- paste0(i,i)
  n=n+1
}
newvector

#Exercise 1
for (i in 1:7) {
  print(i^3)
  
}

#Exercise 2
iris
for (i in colnames(iris)) {
  x <- paste0(i," (", nchar(i), ")")
  print(x) 
}

#Exercise 3
i <- 1
while (i < 6) {
  print(i)
  i=i+1
}
i=0
while (i < 1) {
  print(i)
  i <- rnorm(1)
}

#Exercise 4
coin_flips <- c('heads','tails')
for (i in 1) {
i <- sample(coin_flips, size = 20, replace = TRUE)
  print(i)
}

#Exercise 5
x <- seq(1:20)

n=1
i=1
while (i < 100000) {
  i <- i*n
  n=n+1
  print(i)
}

#### Cran r for packages ####
#### PLOTS ####

library(tidyverse)
iris
# 1. Give a data frame
# 2. Map column names to various aspects of plot
# 3. Give it geoms (things to draw) (geometry)
ggplot(iris,aes(x=Petal.Length,
                y=Petal.Width,
                color=Species,)) +
  geom_point(alpha=0.4,
             size=0.5) +
  geom_smooth(method = "lm",
              se = FALSE) + 
  geom_smooth (method = "lm",
               color='black',
               linetype=3,
               se=FALSE,
               alpha=.3) +
  scale_color_viridis_d(begin=.3) +
  theme_minimal() +
  labs(x="Petal Length",
       y="Petal Width",
       color="Species of iris") +
  facet_wrap(~Species)
  
#loess curve is default
#lm is linear model "y=mx+b"
#alpha is opacity 0 is invisible 1 is opaque.
#linetype-2 makes it a dotted line
#se gets rid of grey error bars when FALSE
# ~ (tilda) means "as a function of"


mpg
ggplot(data=mpg,
       aes(x=manufacturer,
           y=hwy,
           fill=drv)) +
  geom_boxplot(fill='#00054322',color='#800000',alpha=.5) +
  facet_grid(~year,scales='free') +
  theme_bw()

ggplotly(p)

head(iris) %>% 
  kable() %>% 
  kable_classic(lightable_options = 'hover')


pal <- c('green')

scale_color_manual(values = pal)



iris %>% 
  mutate(blink = Sepal.Width < 3.5) %>% 
  ggplot(aes(x=Sepal.Length,y=Sepal.Width,color=Species)) +
  geom_point(data = iris %>% filter(Sepal.Length>3),size=5) +
  geom_point(data = iris %>% filter(Sepal.Length<= 5),color="black")

gganimate::transition_states(blink,state_length = .5) +
gganimate::enter_appear()
anim_save("myanimation.gif")

GGally::ggpairs(iris)
facet_zoom()

library(patchwork)
(p|p2)/p2


#### Tidy Data ####

# 3 rules -->
# Rule 1: Every variable gets its own column
# Rule 2: Every observation gets its own row
# Rule 3: Recantgular data

library(tidyverse)
table1 %>% 
  ggplot(aes(x=year,y=cases,color=country)) +
  geom_path()

table2
table2 %>% 
  pivot_wider(names_from = type,values_from = count)


table3  
table3 %>% 
  separate(rate,into = c("cases","population"),sep="/")

table4a
table4a %>% 
  pivot_longer(cols = c("1999","2000"),names_to = "year")

tidytable4a <- table4a %>% 
  pivot_longer(cols = -country,names_to = "year",values_to = "cases")
  
tidytable4a
tidytable4b<- table4b %>%
  pivot_longer(cols = -country,names_to = "year",values_to = "population")

full_join(tidytable4a,tidytable4b)

#or

full_join(
table4a %>% 
    pivot_longer(cols = -country,names_to = "year",values_to = "cases"),
table4b %>%
    pivot_longer(cols = -country,names_to = "year",values_to = "population")
)

table5
table5 %>% 
  mutate(date=paste0(century,year) %>% 
  as.numeric()) %>% 
  select(-century,-year) %>% 
  separate(rate,into = c("cases","population"),convert = TRUE)


#Popquiz test sewage data
#Cleaning excel data

#for the dates being in excel sep-1 2 and 3 
df1
dates <- janitor::excel_numeric_to_date(as.numeric(df1$site[1:3]))
class(dates)
#pull the month name abbreviation into character
part1 <- lubridate::month(dates,label = TRUE,abbr = TRUE) %>% 
  str_to_upper()
#pull the site numbers from day of month
part2 <- lubridate::day(dates)
#paste them together
paste(part1,part2,sep = "-")
#add them back to dataframe
df1$site[1:3] <- finalproduct



#read in data from excel
df <- read_xlsx("./Exercises/organized dataset.xlsx")

names(df)
df <- clean_names(df) #Cleans names of columns

#getting rid of the space between sewage and pool
df$site <- 
df$site %>% 
  str_replace(" Pool","Pool")

df <- 
df %>% 
  pivot_longer(cols = starts_with("week"),
               names_to = "week",
               values_to = "amount",
               names_prefix = "week_", #getting rid of "week_" in observation
               names_transform = as.numeric) %>% 
  separate(col = "site",into = c("location","site"),sep = " ") #creating new column by separating location and site number

#changing names of sewage pool to SEP and hatchery to HAT
df %>% 
  mutate(location = case_when(location == "SewagePool" ~ "SEP",
                         location == "Hatchery" ~ "HAT"))

####More Tidy Data---------------------

#assignment 6 done in class
df <- read_csv("../Data/BioLog_Plate_Data.csv") %>% 
  clean_names() %>% 
  pivot_longer(starts_with("hr_"),names_to = "hour",values_to = "absorbance",
               names_prefix = "hr_",names_transform = as.numeric) %>% 
  mutate(type = case_when(sample_id == "Soil_1" ~ "Soil",
    sample_id == "Soil_2" ~ "Soil",
    sample_id == "Clear_Creek" ~ "Water",
    sample_id == "Waste_Water" ~ "Water",))

#plot 1
df %>% 
  filter(dilution == 0.1) %>% 
  ggplot(aes(x=hour,y=absorbance,color=type)) +
  geom_smooth(se=FALSE) +
  facet_wrap(~substrate)

#plot 2
df %>%
  filter(substrate == "Itaconic Acid") %>% 
  group_by(dilution,sample_id,hour) %>% 
  summarize(mean_abs = mean(absorbance)) %>% 
  ggplot(aes(x=hour,y=mean_abs,color=sample_id)) +
  geom_path() +
  facet_wrap(~dilution) +
  theme_minimal() +
  transition_reveal(hour)

skim(df)

#Class activity

clean_the_bird_data<- function(x){
  library(tidyverse)
  library(janitor)
  library(skimr)
df <- read.csv(x)
 
df <-  clean_names(df)

skim(df)

#separating into different dataframes 
mass <- df %>% 
  select(-ends_with("_n")) %>% 
  select(-ends_with(c("_wing","_tail","_tarsus","_bill")))

wing <- df %>% 
  select(-ends_with("_n")) %>% 
  select(-ends_with(c("_tail","_tarsus","_bill",
                      "m_mass","f_mass","unsexed_mass")))

tail <- df %>% 
  select(-ends_with("_n")) %>% 
  select(-ends_with(c("_wing","_tarsus","_bill",
                      "m_mass","f_mass","unsexed_mass")))

tarsus <- df %>% 
  select(-ends_with("_n")) %>% 
  select(-ends_with(c("_tail","_wing","_bill",
                      "m_mass","f_mass","unsexed_mass")))

bill <- df %>% 
  select(-ends_with("_n")) %>% 
  select(-ends_with(c("_tail","_wing","_tarsus",
                      "m_mass","f_mass","unsexed_mass")))

mass <- 
mass %>% 
  pivot_longer(cols = c("m_mass","f_mass","unsexed_mass"),
               names_to = "sex",
               values_to = "mass") %>% 
  mutate(sex = sex %>% str_remove("_mass"))


wing <- 
  wing %>% 
  pivot_longer(cols = c("m_wing","f_wing","unsexed_wing"),
               names_to = "sex",
               values_to = "mass") %>% 
  mutate(sex = sex %>% str_remove("_wing"))

tail <- 
  tail %>% 
  pivot_longer(cols = c("m_tail","f_tail","unsexed_tail"),
               names_to = "sex",
               values_to = "mass") %>% 
  mutate(sex = sex %>% str_remove("_tail"))

tarsus <- 
  tarsus %>% 
  pivot_longer(cols = c("m_tarsus","f_tarsus","unsexed_tarsus"),
               names_to = "sex",
               values_to = "mass") %>% 
  mutate(sex = sex %>% str_remove("_tarsus"))

bill <- 
  bill %>% 
  pivot_longer(cols = c("m_bill","f_bill","unsexed_bill"),
               names_to = "sex",
               values_to = "mass") %>% 
  mutate(sex = sex %>% str_remove("_bill"))

clean <- bill %>% 
  full_join(mass) %>% 
  full_join(tail) %>% 
  full_join(tarsus) %>% 
  full_join(wing)

return(clean)
}
saveRDS(clean,"../Data/cleaned_bird_data.rds")




         