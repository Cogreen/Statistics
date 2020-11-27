#+++++++++++++++++++++++++++++++++++++++++++++++#
## 2
#loading files 
setwd("C:\\Users\\HyunjungKim\\Desktop\\2020_1_task")
getwd()
titanic <- read.csv(file="titanic.csv", header = T)
head(titanic)

# # making dummy
# # Class
# titanic$First <- ifelse(titanic$Class=="First", 1, 0)
# titanic$Second <- ifelse(titanic$Class=="Second", 1, 0)
# titanic$Third <- ifelse(titanic$Class=="Third", 1, 0)
# # Age
# titanic$Child <- ifelse(titanic$Age=="Child", 1, 0)
# titanic$Adult <- ifelse(titanic$Age=="Adult", 1, 0)
# # Sex
# titanic$Male <- ifelse(titanic$Sex=="Male", 1, 0)
# titanic$Female <- ifelse(titanic$Sex=="Female", 1, 0)
# # Survivied
# titanic$Survived <- ifelse(titanic$Survived=="Yes", 1, 0)

# making dummy1
titanic_dum <- transform(titanic, 
                         First = ifelse(titanic$Class=="First", 1, 0), 
                         Second = ifelse(titanic$Class=="Second", 1, 0),
                         Third = ifelse(titanic$Class=="Third", 1, 0),
                         Child = ifelse(titanic$Age=="Child", 1, 0),
                         Adult = ifelse(titanic$Age=="Adult", 1, 0),
                         Male = ifelse(titanic$Sex=="Male", 1, 0),
                         Female = ifelse(titanic$Sex=="Female", 1, 0),
                         Survived = ifelse(titanic$Survived=="Yes", 1, 0))
head(titanic_dum)

# making only dummy2
titanic_dum$Class <- NULL
titanic_dum$Age <- NULL
titanic_dum$Sex <- NULL

head(titanic_dum)

## prediction model
# logistic regression model
#summary(fit.setp)

#################################################
### Analysis: glm1
summary(fit.step)
glm(formula = Survived ~ First + Second + Third + Child + Adult 
    + Male + Female, family = binomial, data= titanic_dum)

# ajusting step; direction 
fit.all = glm(Survived~., data=titanic_dum)
fit.step = step(fit.all, direction = "both")
fit.step$anova


## Prediction table
p = predict(fit.step, newdata=titanic_dum, type="response")
threshold = 0.5
yhat = ifelse( p > threshold, 1, 0)
class.tab = table(titanic_dum$Survived, yhat, dnn=c("Actual", "Predicted"))
class.tab


## Predictions 
# Prediction Accuacy 
sum(titanic$Survived == yhat)/length(titanic_dum$Survived)
# Misclassification Rate
sum(titanic$Survived != yhat)/length(titanic_dum$Survived)
# Specificity 
class.tab[1,1]/apply(class.tab, 1, sum)[1]
# Sensitivity 
class.tab[2,2]/apply(class.tab, 1, sum)[2]

# ROC Curve
install.packages("ROCR")
library(ROCR)

pred <- prediction(p, titanic_dum$Survived)
pref <- performance(pred, "tpr", "fpr")
plot(pref, lyt=1, col=2, xlim=c(0,1), ylim=c(0,1),
     xlab="1-Specificity", ylab="Sensitivity", main="ROC Curve")
lines(x=c(0,1), y=c(0,1), col="grey")

# AUC
performance(pred, "auc")@y.values

#################################################
### Analysis: glm2; without adult and female 
glm(formula = Survived ~ First + Second + Third + Child + Male, 
    family = binomial, data= titanic_dum)

# ajusting step; direction 
fit.all = glm(Survived~., data=titanic_dum)
fit.step = step(fit.all, direction = "both")
summary(fit.step)
fit.step$anova


## Prediction table
p = predict(fit.step, newdata=titanic_dum, type="response")
threshold = 0.5
yhat = ifelse( p > threshold, 1, 0)
class.tab = table(titanic_dum$Survived, yhat, dnn=c("Actual", "Predicted"))
class.tab


## Predictions 
# Prediction Accuacy 
sum(titanic$Survived == yhat)/length(titanic_dum$Survived)
# Misclassification Rate
sum(titanic$Survived != yhat)/length(titanic_dum$Survived)
# Specificity 
class.tab[1,1]/apply(class.tab, 1, sum)[1]
# Sensitivity 
class.tab[2,2]/apply(class.tab, 1, sum)[2]

# ROC Curve
install.packages("ROCR")
library(ROCR)

pred <- prediction(p, titanic_dum$Survived)
pref <- performance(pred, "tpr", "fpr")
plot(pref, lyt=1, col=2, xlim=c(0,1), ylim=c(0,1),
     xlab="1-Specificity", ylab="Sensitivity", main="ROC Curve")
lines(x=c(0,1), y=c(0,1), col="grey")

# AUC
performance(pred, "auc")@y.values

#################################################
### Analysis: glm3; with interaction  
# codes in class
# fit.logit3 = glm(formula = Survived ~ Class + Age + Sex + Class:Sex +
#         Age:Sex + Class:Age:Sex, family = binomial(link="logit"), data= titanic)
# glm.aov<-aov(formula = Survived ~ Class + Age + Sex + Class:Sex +
#         Age:Sex + Class:Age:Sex, data=titanic)



#################################################
### Analysis: Tree model
#loading files 
setwd("C:\\Users\\HyunjungKim\\Desktop\\2020_1_task")
getwd()
titanic <- read.csv(file="titanic.csv", header = T)
head(titanic)
summary(titanic)


#################################################
### Analysis: Tree model-CART model 
library(rpart)

# making nodes: minsplit = 1 
my.control <- rpart.control(xval=10, cp=-0.01, minsplit = 1)
fit.titanic <- rpart(Survived~., data=titanic, method="class", control=my.control)
print(fit.titanic) 

# prunning; cp=0
fit.prun.titanic <- prune(fit.titanic, cp= 0.0)
print(fit.prun.titanic)

# drawing tree model
plot(fit.prun.titanic, uniform=T, compress=T, margin= 0.1)
text(fit.prun.titanic, use.n=T, col="blue")

#+++++++++++++++++++++++++++++++++++++++++++++++#

## 3
#loading data
iris
head(iris)
#factor(iris$Sepal.Length)
summary(iris)

#################################################
### Analysis: Tree model-CART model 
library(rpart)


# making nodes: minsplit = 1 
my.control <- rpart.control(xval=10, cp=0, minsplit = 5)
fit.iris <- rpart(Species~., data=iris, method="class", control=my.control)
print(fit.iris) 

# chekcing all model considering cp
printcp(fit.iris)

# prunning; cp=0.012
fit.prun.iris <- prune(fit.iris, cp= 0.012)
print(fit.prun.iris)

# drawing tree model    
plot(fit.prun.iris, uniform=T, compress=T, margin= 0.1, branch=0.2)
text(fit.prun.iris, use.n=T, col="blue") #fancy=T, fweight=0.5, fheight = 0.5

#$$$$$$$$$$$$$$$$$$$$$$$$$$$
# prediction 
newdata <- data.frame()
predict(fit.prun.iris, newdata, type=c("vector","prob", "class","matrix")
        yhat = ifelse( p > threshold, 1, 0)
        class.tab = table(titanic_dum$Survived, yhat, dnn=c("Actual", "Predicted"))
        class.tab
        
        
        ## Predictions 
        # Prediction Accuacy 
        sum(titanic$Survived == yhat)/length(titanic_dum$Survived)
        # Misclassification Rate
        sum(titanic$Survived != yhat)/length(titanic_dum$Survived)
        # Specificity 
        class.tab[1,1]/apply(class.tab, 1, sum)[1]
        # Sensitivity 
        class.tab[2,2]/apply(class.tab, 1, sum)[2]
        
        

#+++++++++++++++++++++++++++++++++++++++++++++++#
## 4
# making variables 
# make data
data1 <- data.frame()
data1 = edit(data1)
str(data1)
    
data2 <- data.frame()
data2 = edit(data2)
str(data2)

# x11x21 <- sum(data1[1,1],data2[1,1]) ;x11x21  #3
# x11x22 <- sum(data1[2,1],data2[2,1]) ;x11x22  #2
# x11x23 <- sum(data1[3,1],data2[3,1]) ;x11x23  #6
# 
# x21x21 <- sum(data1[1,2],data2[1,2]) ;x21x21  #2
# x21x22 <- sum(data1[2,2],data2[2,2]) ;x21x22  #11
# x21x23 <- sum(data1[3,2],data2[3,2]) ;x21x23  #1
# 
# x31x21 <- sum(data1[1,3],data2[1,3]) ;x31x21  #4
# x31x22 <- sum(data1[2,3],data2[2,3]) ;x31x22  #7
# x31x23 <- sum(data1[3,3],data2[3,3]) ;x31x23  #3

# sum
sum(data1)  #18
sum(data2)  #21


##################################################
## after 2020-07-09 ############# 4 ##############
##################################################
# GINI index 
gini_y1 = 1-((3/sum(data1))^2+(2/sum(data1))^2+(4/sum(data1))^2+
                 (0/sum(data1))^2+(6/sum(data1))^2+(1/sum(data1))^2+
                 (0/sum(data1))^2+(0/sum(data1))^2+(2/sum(data1))^2)
gini_y1

gini_y0 = 1 - ((0/sum(data2)^2+(0/sum(data2))^2)+(2/sum(data2))^2+
                   (2/sum(data2))^2+(5/sum(data2))^2+(0/sum(data2))^2+
                   (4/sum(data2))^2+(7/sum(data2))^2+(1/sum(data2))^2)
gini_y0



## CART-tree
# load library
library(rpart)
my.control <- rpart.control(xval=10, cp=0, minsplit=nrow(data1)*0.05)
fit.data1 = rpart()

library(MASS)
head(Boston)

##################################################
## after 2020-07-11 ############# 4 ##############
##################################################
#loading files 
setwd("C:\\data")
getwd()
data <- read.csv(file="mid44.csv", header = T)
head(data)

# making nodes: minsplit = 1 
library(rpart)
my.control <- rpart.control(xval=10, cp=0, minsplit = 5)
fit.iris <- rpart(癤퓓~., data=data, method="class", control=my.control)
print(fit.iris) 


#+++++++++++++++++++++++++++++++++++++++++++++++#
##5
 
