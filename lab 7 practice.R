## Group 4
## Name: John Ahn
## Group members: Dolores Juarez, Mitik Merchant, Nick Su 
##

## QTM 100 Lab
## Date: 10/8/2020
## Lab Practice 6

########## Q1
#Set working directory
setwd("C:/Users/ASUS X571LI/Desktop/QTM LAB/Datasets/")

#Read in the data
gardasil<-read.table("gardasil.txt",header=TRUE)

##### a)
# View contingency table with proportions
prop.table(table(gardasil$LocationType))

### Answer: 31.8% went to the urban clinic 

##### b)
# View contingency table with proportions
prop.table(table(gardasil$LocationType))

### Answer: 68.2% went to suburban clinic


########## Q2

##### a)
### Answer: 
# We need to run one sample z-test to determine whether our sample
# has a statistically different percentage of suburban patients.

##### b)
### Answer:
# H0: Our sample has the percentage of suburban patients that is not 
#     statistically different from that of the general population, 0.7.
# HA: Our sample has the percentage of suburban patients that is 
#     statistically different from that of the general population, 0.7.

##### c)
#View contingency table with frequencies
table(gardasil$LocationType)

#Run one sample z test on a proportion
prop.test(963, 963+450,p=0.7,correct=F)

#Square root the X^2
sqrt(2.2957)

### Answer: 
# The p-value is 0.1297
# The z test statistic, (sqrt(x^2)), is 1.515
# The confidence interval is  (95% CI: 65.7% - 70.5%)

##### d)
### Answer:
# Because the p-value is greater than 0.05, we fail to reject the H0. 
# The data do not provide strong evidence indicating that the percentage of 
# suburban patients of the sample is different from that of researcher's 
# expectation.


########## Q3
##### a)
### Answer: we could use the variable named "AgeGroup"

##### b)
# View contingency table with proportions
prop.table(table(gardasil$AgeGroup))

### Answer: 
# 49.6% of the women who received the vaccine in the sample is under the
# age of 18.

##### c)
### Answer: 
# H0: Our sample has the percentage of women under the age of 18 that is not 
#     statistically different from that of researcher's knowledge, 0.53.
# HA: Our sample has the percentage of women under the age of 18 that is 
#     statistically different from that of researcher's knowledge, 0.53.

##### d)
#View contingency table with frequencies
table(gardasil$AgeGroup)

#Run one sample z test on a proportion
prop.test(701, 701+712,p=0.53,correct=F)

#Square root the X^2
sqrt(6.5159)
# 2.552626

### Answer: 
# The p-value is 0.011
# The z test statistic, (sqrt(x^2)), is 2.55
# The confidence interval is  (95% CI: 47.0% - 52.2%)

##### e)
### Answer: 
# We are 95% confident that our true population parameter will be 
# between 0.47 and 0.522. 
# Because this confidence interval does not include 0.53, we reject the H0.
# The data provide strong evidence indicating that the percentage of 
# women under the age of 18 of the sample is different from that of researcher's 
# knowledge, 0.53.

########## Q4
# calculate the p-value all together and correct for rounding errors
2*(1-pnorm(sqrt(6.5159)))

### Answer: the p-value is 0.011. 
#   Since we found the X-squared value, we can use this to 
#   find the probability that a vaue would be found this 
#   many z-scores away from the p. We will use the pnorm 
#   funchtion becasue we are dealing with a normal 
#   distribution. We will 1 - pnorm because we want to find
#   value of the upper tail. We use he squareroot of the 
#   X-squared value to avoid rounding errors. We multiply 
#   everything by two to factor in the lower tail.

########## Q5
### Answer: 
# Yes, the Johns Hopkins team should be worried. The result in Q3 shows that 
# their sample of girls for the Gardasil vaccine study is not perfectly 
# representative of the local population. 



