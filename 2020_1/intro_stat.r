## 1 
tree <- c(70, 65, 63, 72, 81, 83, 66, 75, 80, 75, 79, 76, 76, 69, 75, 74, 85, 86, 71, 64, 78, 80, 74, 72, 77, 81, 82, 80, 80, 80, 87)
#1-1
hist(tree, col="seagreen4", main = "Trees in Arboretum", labels=T, cex=0.2)
#1-2
stem(tree, scale=0.5)

##2 
male <- c(49, 58, 62,86,82, 72,	40, 52, 65,	45, 56, 60,	48, 50, 64,	93, 85, 70,	97, 80, 78,	58, 60, 67,	58, 62, 69,	98, 80, 88)	
female <- c(60, 68,	72, 72,	66, 67,	65, 61,	75, 62,	78, 72,	62, 79,	64, 71,	74, 74,	58, 73)

#2-1
hist(male, col="dodgerblue", main="Grades of Male", xlab="Male", ylab="Grade",labels=T, breaks=10)
hist(female, col="palevioletred2", main="Grades of Female", xlab="Female", ylab="Grade", labels=T, breaks=5)

#2-2

#2-3
mean(male)
sd(male)
male_cv <- 100*sd(male)/mean(male)
male_cv

mean(female)
sd(female)
female_cv <- 100*sd(female)/mean(female)
female_cv

## 3
# 2nd St 
total <- 120
advanced <- 40
transfer <- 80

advanced$math 
transfer$math 20

#3-1
(20/80)/(80/120)
  
#3-2
(4/24)/(40/120)


## 4 
sample <- c(68, 70, 70, 71, 69, 74, 71, 72, 70, 73)
length(sample)
mean(sample)
var(sample); sd(sample)

#4-2
x <- rnorm(length(sample), mean(sample), sd(sample))
x

# confidence interval: 95%
t.test(x,conf.level = 0.95)

# confidence interval: 99%
t.test(x,conf.level = 0.99)


## 5
# 5-1 
x <- rnorm(1000)
t.test(x)

prop.test(600,1000)
binom.test(600,1000)

5. 새로운 교육정책을 수립하고 이 정책에 대한 여론조사를 실시하였다. 우리나라 성인 1000명을 임의로 추출하여 조사한 결과, 600명이 찬성한다고 대답하였다. 
(1) 우리나라 성인 중 새로운 교육정책에 찬성하는 비율의 추정값을 구하라. 
(2) 95% 신뢰구간을 구하여라. 
(3) 위에서 구한 신뢰구간의 의미를 설명하여라.

