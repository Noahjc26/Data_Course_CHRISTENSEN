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