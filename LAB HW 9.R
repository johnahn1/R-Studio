# Group 4: Dolores Juarez, John Ahn, Nick, Mitich 
# QTM 100 Lab
# Lab Instructor: Sena Ageza 
# Lab Practie 9
# Oct 22, 2020 

########################## QUESTION 1 ####################################

# set working directory 
setwd("C:/Users/Dolores Juarez/Documents/QTM 100/lab_datasets")

# Import lead dataset 
Lead <- read.csv("lead.csv", header = TRUE)

### PART A ###

# Find the mean IQ of the children in the sample 
mean(Lead$Iqf)

# Find the 
sd(Lead$Iqf)

### ANSWER ###

# Mean = 91.08
# SD = 14.40

### PART B ###

# Create a histogram of the IQ scores 
hist(Lead$Iqf)

### ANSWER ###

# The IQ is seems to have a normal distribution. The population is 
# greater than 30 

######################### QUESTION 2 #################################

### PART A ###

# A one-sample two-sided t-test should be run 

### PART B ###

# Ho : mu equals 85
# HA : mu does not equal 85

### PART C ### 

# Run a one-sample t-test 
t.test(Lead$Iqf, mu = 85)

### ANSWER ###

# Test statistic = 4.7
# P-value = 6.825e-06

### PART D ###

# Confidence Interval = (88.52, 93.64)

### PART E ###

# We reject the null hypothesis since the p-valaue is less than 0.05.
# There is sufficient evidence that the true IQ mean is not 85. 85 is 
# not included in the 95% confidence interval for our sample mean, and
# 85 is 4.7009 t-test statiscics away from the mu nought 

######################### QUESTION 3 #############################

### PART A ### 

# one-sample t-test

### PART B ###

# Ho : mu equals 36
# HA : mu does not equal 36

### PART C ###

# Run a one-sample t-test for the average blood lead levels 
t.test(Lead$Ld72, mu = 36)

### ANSWER ###

# Test Statistic = 1.1502
# P-value = 0.2524

### PART D ###

# Confidence Interval = (32.199, 37.0076)

### PART E #

# Since the p-value is grater than 0.05, we fail to reject the null
# hypothesis. There is not sufficient evidence to believe the true mean 
# blood lead level does not equal 36. This value is present with the 
# 95% confidence interval of the sample mean(34.6). The sample mean 
# is only 1.15 t statistics away from the mean, an insignificant amount
