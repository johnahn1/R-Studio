## Group 4
## Name: John Ahn
## Group members: Dolores Juarez, Mitik Merchant, Nick Su 
##

## QTM 100 Lab
## Date: 10/15/2020
## Lab Practice 8

#Set working directory
setwd("C:/Users/ASUS X571LI/Desktop/QTM LAB/Datasets/")
#Read in the data
pharynx = read.csv("pharynx.csv", header = T)

summary(pharynx$TIME)

########## Q1
##### (a)
#Create a variable that indicates whether each observation survived >= 500 days
pharynx$Survived_500 = factor(NA, levels=c("yes","no"))

#Re-assign values
pharynx$Survived_500[pharynx$TIME >= 500] = "yes"
pharynx$Survived_500[pharynx$TIME < 500] = "no"

#Check re-coding of Survived_500
table(pharynx$Survived_500)

##### (b)
### Answer: 
# Chi-Square Test and Two Sample z test could be used to investigate if there 
# is an association between survival past 500 days and treatment group.

# Hypothesis for chi square test
# H0: Survival past 500 days and treatment group are independent 
# HA: Survival past 500 days and treatment group are dependent

# Hypothesis for Two-sample z-test
# H0:The proportion of cancers that survived past 500 days is equal to 0.5
# HA:The proportion of cancers that survived past 500 days is not equal to 0.5

# chi sqaure test is appropriate for...


##### (c)
#Chi-Square Test for survival past 500 days and treatment group
Surv_TX_test = chisq.test(pharynx$Survived_500, pharynx$TX, correct=F)
Surv_TX_test

### Answer:
# P-value, 0.1607, is large enough that we fail to reject the H0. 
# The data provide convincing evidence to suggest that Survival past 500 days 
# and treatment group are not associated.

##### (d)
### Answer:
# We have not tested if our conditions are satisfied to perform the chi squre test.


########## Q2
##### (a)
### Answer: 
# All expected counts should be at least 5.

##### (b)
# assumption regarding expected cell counts
Surv_TX_test$expected

### Answer:
# The assumption regarding expected cell counts is satisfied for the 
# previous test regarding the association between survival past 500 days and 
# treatment group because all cell counts are above 5.

##### (c)
#Chi-Square Test for survival past 500 days and stage of the tumor
Surv_tstage_test = chisq.test(pharynx$Survived_500, pharynx$T_STAGE, correct=F)
Surv_tstage_test

# assumption regarding expected cell counts
Surv_tstage_test$expected

### Answer:
# The assumption is violated because expected counts for primary tumor 
# measuring 2 cm or less in largest diameter are less than 5. 
# 2 sample z-test should be appropriate as an alternative test because its 
# conditions do not include the size of the  sample size.

########## Q3
#Calculations 
1-pchisq(1,df=1)
1-pchisq(3,df=1)
1-pchisq(5,df=1)
1-pchisq(1,df=2)
1-pchisq(3,df=2)
1-pchisq(5,df=2)

### Answer:
# a) p-value = 0.317, not significant
# b) p-value = 0.0833, not significant
# c) p-value = 0.0253, significant
# d) p-value = 0.607, not significant
# e) p-value = 0.223, not significant
# f) p-value = 0.0821, not significant


# As the test statistic increases, the p-value (decreases). Therefore, larger test 
# statistics present (more) evidence against the null hypothesis. The same test 
# statistic value with different degrees of freedom (can) result in a different 
# conclusion for a specified level of significance (e.g., ?? = 0.05).

