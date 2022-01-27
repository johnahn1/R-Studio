# Group 4 : Dolores Juarez, Nick, Mitich, John 
# QTM 100 Lab 
# Lab instructor : Sena Agezo 
# Lab HW 10 

# Set the working directory 
setwd("C:/Users/Dolores Juarez/Documents/QTM 100/lab_datasets")

# Import YRBSS data
YRBSS <- read.csv("yrbss2013.csv", header = TRUE)
summary(YRBSS$height_m)

# Import Functions 
source("TestingFunctions.R")

####################### QUESTION 1 ##############################

### PART A ###  

# View the summary of height 
summary(YRBSS$height_m)

# Histogram of Height
hist(YRBSS$height_m, main = "Height", xlab = "Meters")
# The distribution of the population height is normal centered
# at the mean 

# Find the Mean 
mean(YRBSS$height_m)
# the true proportion mean is 1.68729

# Find the standard deviation 
sd(YRBSS$height_m)
# the true proportion standard deviation is 0.103

### PART B ###

# The data does provide a random sample of the observations 
# from the population because the function inference.mean
# randomly selects observations from the population. This 
# also makes the observations independent. The sampling
# distribution of the sample mean would be normally distributed

### PART C ###

# We are testing H0 : mu equals 1.687 versus Ha :mu does not 
# equal 1.687. In the hypothesis test, we run the
# risk of committing a Type II error because in reality the 
# null hypothesis is actually false. The targeted Type I 
# error rate is 0.05 and the targeted confidence interval 
# coverage is 95% Because sampling distribution assumptions 
# are satisfied, we expect the observed Type I error rate 
# and confidence interval coverage from simulation results
# to equal the targeted levels.


### Part D ###

# inference for 100 samples of size n = 20 from the height_m
# variable at the a = 0.05 level of significance.
SIM_H_20 <- inference.means(variable = YRBSS$height_m,
                        sample.size = 20,
                        alpha = 0.05,
                        num.reps = 100)

# Histogram of the sample means 
hist(SIM_H_20$samp.est, main = "Sample Means")

# the sample means appear to be normally distrubuted about the 
# data mean of 1.687

# Histogram of T Test Statistics 
hist(SIM_H_20$test.stat, main = "T test Statistic")
# The t test statistics appear to be normally distributed 
# about 0 

# Histogram of the sample p-values
hist(SIM_H_20$p.val)
# There seems to be large variation in the p-values

# Contingency Table of samples that captured 1.687 in there 
# confidence interval and the decisions on Ho 
table(SIM_H_20$capture, SIM_H_20$decision)
# there are 4 samples that rejected the Ho and did not have
# 1.687 in their confidence interval. If the Ho was actually
# true, these samples would have committed a type I error

### PART E ###

# Plot of Confidence intervals
plot.ci(results = SIM_H_20, true.val = 1.687)

# Table of Confidence intervals 
table(SIM_H_20$capture)
# 96 percent of samples capture the true parameter vlaue 
# From the plot, we can see that the confidence intervals all
# appear to be of varying lengths, but upon closer inspection,
# they are indeed the same length, they just happen to have 
# different interval, some more towards one side of the mean 
# than others. They seem to be randomly scattered about the 
# true mean. 

### PART F ###








######################### QUESTION 2 ##########################

### Part A ###

# Histogram of Days Smoke variable
hist(YRBSS$days_smoke, main = "how many days in the past 30 have you smoked",
     xlab = "Days")
# the data is right skewed with a few outliers

# Mean of the Days Smoke variable
mean(YRBSS$days_smoke)
# the true proportion mean is 1.496

# Standard deviation of Days Smoke variable
sd(YRBSS$days_smoke)
# the true proportion standard deviation is 5.701

### PART B ###

# The data does provide a random sample of the observations 
# from the population because the function inference.mean
# randomly selects observations from the population. This 
# also makes the observations independent. However, the 
# sampling distribution of the sample mean would not be 
# normally distributed and the sample size is less than 20.
# The assumptions are not satisfied.

### PART C ###

# We are testing H0 :mu equals 1.496 versus Ha : mu does 
# not equal 1.496. In the hypothesis test, we run the risk 
# of committing a Type II error because in reality the null 
# hypothesis is actually false The targeted Type II error 
# rate is not 0.05 and the targeted confidence interval
# coverage is 95%. Because sampling distribution assumptions 
# are not satisfied, we expect the observed Type I error rate
# and confidence interval coverage from simulation results
# to not equal the targeted levels

### PART D ###

# Inference for 100 samples of size n = 20 from the days smoke
# variable at the alpha = 0.05 level of significance. 
SIM_DS_20 <- inference.means(variable = YRBSS$days_smoke,
                        sample.size = 20,
                        alpha = 0.05,
                        num.reps = 100)

# Histogram of the sample means 
hist(SIM_DS_20$samp.est, main = "Sample Means of Days Smoke")
# the sample means are right skewed 

# Histogram of T Test Statistic 
hist(SIM_DS_20$test.stat, main = "t test statistic of smoke days")
# the t test statistics are left skewed with outliers

# Histogram of sample p-values 
hist(SIM_DS_20$p.val, main = "P-Value of Smoke Days")
# the data is very variable with a significant portion that is 
# less than 0.05. The mode is 0.0 to 0.1

# View the results of the hypothesis test 
table(SIM_DS_20$decision)
# 62 samples failed to reject Ho. If in fact Ho was false, 
# 62% of samples would have committed a type two error
# 32 samples rejected Ho. If Ho is in fact true, 32% of samples 
# would have committed a type one error

### PART E ###

# create plot of confidence intervals for Days Smoke samples
plot.ci(results = SIM_DS_20, true.val = 1.496227)

# Contingency table with decision on Ho and Confidence Interval
table(SIM_DS_20$capture, SIM_DS_20$decision)
# 68% of samples capture the samples capture the true parameter
# value. The intervals seem to be of different sizes Some are
# very large while others are very short.


######################## QUESTION 3 ##########################

# Inference for 10 000 samples of size n = 20 from the height 
# variable at the alpha = 0.05 level of significance. 
SAM_H_20 <- inference.means(variable = YRBSS$height_m,
                             sample.size = 20,
                             alpha = 0.05,
                             num.reps = 10000)

# Plot of Confidence interval
plot.ci(results = SAM_H_20, true.val = 1.687)

# Table of confidence interval where mu is captured 
table(SAM_H_20$capture)

# Inference for 10 000 samples of size n = 50 from the height
# variable at the alpha = 0.05 level of significance. 
SAM_H_50 <- inference.means(variable = YRBSS$height_m,
                            sample.size = 50,
                            alpha = 0.05,
                            num.reps = 10000)

# Plot of Confidence interval
plot.ci(results = SAM_H_50, true.val = 1.687)

# Table of confidence interval where mu is captured 
table(SAM_H_50$capture)

# Inference for 10 000 samples of size n = 100 from the height 
# variable at the alpha = 0.05 level of significance. 
SAM_H_100 <- inference.means(variable = YRBSS$height_m,
                            sample.size = 100,
                            alpha = 0.05,
                            num.reps = 10000)

# Plot of Confidence interval
plot.ci(results = SAM_H_100, true.val = 1.687)

# Table of confidence interval where mu is captured 
table(SAM_H_100$capture)

# height_m                        n=20       n=50      n=100
# Assumptions satisfied? (y/n)    yes        yes        yes
# Observed Type I error rate     4.61%       4.91%     5.06%
# Observed C.I. coverage        95.39%      95.09%     94.94%    
# Valid inference? (y/n)          yes        yes        yes 

#############################################################


# Inference for 10 000 samples of size n = 20 from the days 
# smoke variable at the alpha = 0.05 level of significance. 
SAM_DS_20 <- inference.means(variable = YRBSS$days_smoke,
                             sample.size = 20,
                             alpha = 0.05,
                             num.reps = 10000)

# create plot of confidence intervals for Days Smoke samples
plot.ci(results = SAM_DS_20, true.val = 1.496227)

# Table of confidence interval where mu is captured 
table(SAM_DS_20$capture)

# Inference for 10 000 samples of size n = 50 from the days 
# smoke variable at the alpha = 0.05 level of significance. 
SAM_DS_50 <- inference.means(variable = YRBSS$days_smoke,
                             sample.size = 50,
                             alpha = 0.05,
                             num.reps = 10000)

# create plot of confidence intervals for Days Smoke samples
plot.ci(results = SAM_DS_20, true.val = 1.496227)

# Table of confidence interval where mu is captured 
table(SAM_DS_50$capture)

# Inference for 10 000 samples of size n = 100 from the days 
# smoke variable at the alpha = 0.05 level of significance. 
SAM_DS_100 <- inference.means(variable = YRBSS$days_smoke,
                             sample.size = 100,
                             alpha = 0.05,
                             num.reps = 10000)

# create plot of confidence intervals for Days Smoke samples
plot.ci(results = SAM_DS_20, true.val = 1.496227)

# Table of confidence interval where mu is captured 
table(SAM_DS_100$capture)

# height_m                        n=20       n=50      n=100
# Assumptions satisfied? (y/n)     no        yes        yes 
# Observed Type I error rate     31.56%     12.65%     8.73%
# Observed C.I. coverage         68.44%     87.35%     91.27%    
# Valid inference? (y/n)          no          no          no 

####################### QUESTION 4 ##########################

# No, the rate of confidence intervals that capture the true 
# parameter of value does not improve. They are very similar
# to each other. This is likely because the height is ready 
# normally distributed.

# Even when the assumption are satisfied, we do not always have
# a valid inference. I think that the central limit theorem 
# is a good suggestion, but not always reliable. Therefore, it
# is very important that all the assumptions are satisfid