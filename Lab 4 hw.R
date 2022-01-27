#
#
#
#Lab 4
#
#2020-9-17
#
setwd("C:/Users/Nick/Desktop/20 Fall/QTM/Lab/Data")
fruitfly = read.csv("fruitfly.csv", header = TRUE)
#load fruitfly dataset

summary(fruitfly)

#Q1

fruitfly$typeCat <- factor(x = fruitfly$type,
                          labels = c("Type1 no females",
                                     "Type2 1 newly pregant female",
                                     "Type3 8 newly pregant female",
                                     "Type4 1 virgin female",
                                     "Type5 8 virgin female"))
#convert into categorical data

#Q1a

boxplot(fruitfly$lifespan ~ fruitfly$type,
        main = "Distribution of lifespan among 5 types",
        xlab = "Type",
        ylab = "lifespan (Days)")
#create a boxplot showing the distribution of lifespan according to group

#Q1b

##Type 5 have the lowest average lifespan

tapply(X = fruitfly$lifespan, INDEX = fruitfly$typeCat, FUN = mean)
#compute the average value of lifespan among the groups

tapply(X = fruitfly$lifespan, INDEX = fruitfly$typeCat, FUN = sd)
#compute the standard deviation for the 5 groups

##Type 5 8 virgin female: mean lifespan = 38.72 days, 
##standard deviation = 12.10


#Q2a

pnorm(q = 30, mean = 38.72, sd = 12.10)
#probability of surviving 30 days is 0.236

diff(pnorm(q = c(30,50), mean = 38.72, sd = 12.10))
#probability of surviving 30-50 days is 0.589

diff(pnorm(q = c(50,70), mean = 38.72, sd = 12.10))
#probability of surviving 50-70 days is 0.171

pnorm(q = 70, mean = 38.72, sd = 12.10, lower.tail = FALSE)
#probability of surviving more than 70 days is 0.005

# days         Type 3        Type 5
# <=30         0.01          0.236
# 30-50        0.17          0.589
# 50-70        0.50          0.171
# >70          0.32          0.005

#Q2b

mean(c(81,65,56,70,56))
sd(c(81,65,56,70,56))

##It would more likely come from Type 3 "8 newly pregant females" 
##because the group has an average value of 65.6 with stdev of 10.5

#Q3
fruitflysubset<-subset(fruitfly,type==5)
mean(fruitflysubset$lifespan)
sd(fruitflysubset$lifespan)

#theoretical vs Observed

qnorm(p = 0.1, mean = 38.72, sd = 12.10)
#23.2
quantile(x = fruitflysubset$lifespan, probs = 0.1)
#21.8

qnorm(p = 0.25, mean = 38.72, sd = 12.10)
#30.6
quantile(x = fruitflysubset$lifespan, probs = 0.25)
#32

qnorm(p = 0.5, mean = 38.72, sd = 12.10)
#38.7
quantile(x = fruitflysubset$lifespan, probs = 0.5)
#40

qnorm(p = 0.75, mean = 38.72, sd = 12.10)
#46.9
quantile(x = fruitflysubset$lifespan, probs = 0.75)
#47

qnorm(p = 0.9, mean = 38.72, sd = 12.10)
#54.2
quantile(x = fruitflysubset$lifespan, probs = 0.9)
#54