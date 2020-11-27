## 1-3
#1
set.seed(1234)
rt(100, 5)

#2 
hist(rt(100, 5), col="turquoise")

#3
boxplot(rt(100, 5), col="lightblue2")

#4
stem(rt(100, 5))


## 1-5
library(HSAUR2)
library(MVA)
data(USairpollution)
head(USairpollution)

# 1-5; round; pairs
dim(USairpollution)
air = USairpollution[(1:41),(1:7)]
round(cor(air),3)
pairs(air)

# 1-5; starplot
library(MASS)
#data.frame(air)
#city <- row.names.data.frame(air); head(city)
stars(air)

# 1-5; round; pairs
dim(USairpollution)
air <- data.frame(USairpollution)
air = USairpollution[(1:41),(1:7)]
round(cor(air),3)
pairs(air)

# 1-5; starplot
library(MASS)
stars(air)

# 1-5; faceplot
library(aplpack)
face(air)


## 2
# 2-1
install.packages("ade4")
library("ade4")
data(deug)
data <- deug$tab
data

# 2-1-1
summary(data)

# 2-1-2
round(cor(data), 3)
plot(data, pch=1)

# 2-1-3
screen(data)

# 2-1-3
# covariance matrix
data_cov <- cov(data)
data_cov 
# eigne decomposition
data_eigen <- eigen(data_cov)
data_eigen


# 2-1-6
#summary(princomp(data, cor=T))
library(stats)
data.pca = princomp(data, cor=T, scores=T)
names(data.pca)
data.pca


## 2-4
# loading data; USArrests
USArrests
head(USArrests)

# 2-4-1
arrest <- USArrests
library(stats)
arrest.pca = princomp(arrest.data, cor=T, scores=T)
arrest.pca
summary(arrest.pca)
arrest.pca$loadings[,1:2]

## 3
## 3-2
#loading files 
# install.packages("readxl")
# library(readxl)
# setwd("C:\\Users\\HyunjungKim\\Desktop\\2020_1_task\\")
# getwd()
# fav_sub <- readxl::read_excel(path="C:\\Users\\HyunjungKim\\Desktop\\2020_1_task\\favoritesujects.xlsx", sheet="favoritesujects", col_namesT)

## 3
## 3-2
#loading files 
setwd("C:\\Users\\HyunjungKim\\Desktop\\2020_1_task")
getwd()
fav_sub <- read.csv(file="favoritesujects.csv", header = T)
head(fav_sub)
dim(fav_sub)
fav_sub <-fav_sub[,2:7]

# 3-2-1
install.packages("psychz")
library(psych)
head(fav_sub)
fav_sub.factor <- principal(fav_sub, rotate="none")
names(fav_sub.factor)
fav_sub.factor$values
plot(fav_sub.factor, type="b")
fav_sub.factor$scores

# what are factors? 
library(stats)

fav_sub.none

# 3-2-1; factor rotation  
fav_sub.none <- factanal(fav_sub, factors=2, rotation="none")
fav_sub.none 

fav_sub.Varimax <- principal(fav_sub, nfactors = 2, rotate="varimax")
fav_sub.Varimax

fav_sub.Promax <- principal(fav_sub, nfactors = 2, rotate="promax")
fav_sub.Promax 

# fav_sub.Varimax; Commonality 
BIO = (0.90)^2 + (0.12)^2; BIO
GEO = (0.86)^2 + (0.14)^2; GEO
CHEM = (0.90)^2 + (0.05)^2; CHEM
ALG = (0.00)^2 + (0.89)^2; ALG
CALC = (0.09)^2 + (0.91)^2; CALC
STAT = (0.17)^2 + (0.52)^2; STAT

# fav_sub.Varimax; SS loadings  



# 3-2-4
fav_sub.Varimax$scores
head(fav_sub.Varimax$scores)

biplot(fav_sub.Varimax)


##################################
fav_sub.fact1 = factanal(fav_sub, factors=2, rotation="varimax", scores="Bartlett")
fav_sub.fact1
fav_sub.fact2 = factanal(fav_sub, factors=2, rotation="none")
fav_sub.fact2

# fav_sub.Varimax <- principal(fav_sub, nfactors = 2, rotate="varimax")


library(stats)


fav_sub.fact1 = factanal(fav_sub
                         #       
                         
                         factors=2, rotation="none")
fav_sub.fact1 = factanal(fav_sub, factors=2, rotation="none")

# fav_sub.pca <- princomp(fav_sub, cor=T, scores=T)
# fav_sub.pca


head(fav_sub)

dim(fav_sub)

ek.factor

#summary(fav_sub)
#dim(fav_sub)

# corrleation matrix 
## fav_sub_cor = cor(fav_sub)
## eigen(fav_sub_cor)
## fav_sub_cor = principal(fav_sub, rotate="none")



