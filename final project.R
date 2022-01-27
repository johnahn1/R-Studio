## Group 4
## Group members: John Ahn, Dolores Juarez, Mitik Merchant, Nick Su 

## QTM 100 Lab
## Title: Final Project

# Set working directory 
setwd("C:/Users/ASUS X571LI/Desktop/QTM LAB/Datasets/")

# Import the dataset
heart = read.csv("heart_failure.csv",header=TRUE)
sodium = heart$serum_sodium


### Research Question 1: Chi-square ###
############################  Explanatory Variable  ############################

# Create new variable for different levels
heart$sodiumlvl = factor(NA, levels = c("low", "normal","high"))

# Re-assign values of the new levels 
heart$sodiumlvl[sodium <=135] = "low"
heart$sodiumlvl[sodium >135 & heart$serum_sodium<145] = "normal"
heart$sodiumlvl[sodium >=145] = "high"

# Show the table for the new 
table(heart$sodiumlvl)


########################  Dichotomous response variable  #######################

# Create a new high blood pressure variable 
heart$BP = factor(NA, levels= c("High BP","Not High BP"))

# Re-Assign values of the new HBP variable
heart$BP[heart$high_blood_pressure == 0] <- "Not High BP"
heart$BP[heart$high_blood_pressure == 1] <- "High BP"

table(heart$BP)

# Make a contingency table for blood pressure and level of serum sodium
table(heart$BP, heart$sodiumlvl)

#      low moderate high
# yes  34       66    5
# no   65      123    6

################################### Plots #####################################

# Make a contingency table for blood pressure and level of serum sodium
sodiumTable = table(heart$sodiumlvl, heart$BP)

# Show the contingency table 
sodiumTable

# Add margins to the table
addmargins((sodiumTable))

# View contingency table with proportions
prop.table(sodiumTable, margin = 1)

# Graph the barplot 
graphtable = table(heart$BP, heart$sodiumlvl)
graphprop = prop.table(graphtable, margin = 2)
barplot(graphprop,legend.text = T, 
        beside = T, 
        ylab = "Proportion", 
        ylim = c(0,0.8))


##################################  Analysis  ##################################

########## Hypothesis 
# H0: There is no association between hypertension (high blood pressure) 
#     and level of serum sodium in the blood.

# HA: There is an association between hypertension (high blood pressure) 
#     and level of serum sodium in the blood.


########## Check conditions for chi-square test
# Show the expected cell counts 
chitest_BP_Sodium = chisq.test(heart$BP, heart$sodiumlvl)
chitest_BP_Sodium$expected

##### Result: 
# One of the cell count is less than 5. 
# Thus, the condition for chi-squared test is not satisfied. 


########## Still run chi-square test, Alpha level = 0.05
chitest_BP_Sodium

##### Result: 
# Because the condition is not satisfied, the console suggests that chi-squared 
# approximation may be incorrect
#
# X-squared = 0.54511, df = 2, p-value = 0.7614
#
# Therefore, we also run Fisher's exact test

########### Run Fisher's exact test, Alpha level = 0.05
fisher.test(heart$BP, heart$sodiumlvl)

# p-value = 0.7507

########### Interpretation 
# Because p-value is greater than 0.05, we fail to reject the null hypothesis. 
# There is not sufficient evidence to say that there is an association between 
# a patient's sodium  level and whether or not the patient has high 
# blood pressure. 


##############################################################################

#### Research Question 2: Two sample t-test ####

# Summary of ejection fraction 
summary(heart$ejection_fraction)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14.00   30.00   38.00   38.08   45.00   80.00 

# Boxplot of ejection fraction vs high blood pressure 
boxplot(heart$ejection_fraction ~ heart$BP,
        ylab = "Ejection Percentage",
        xlab = "Presence of High Blood Pressure")

# Mean ejection fraction of high blood pressure groups 
tapply(X=heart$ejection_fraction, INDEX = heart$BP, FUN = mean)

#High BP      Not High BP 
#38.47619    37.87113 

# Mean ejection fraction of high blood pressure groups 
tapply(X=heart$ejection_fraction, INDEX = heart$BP, FUN = mean)

#High BP Not High BP 
#38.47619    37.87113 

################################### Plots #####################################

# Create histogram of the ejection fraction data for each group 
hist(heart$ejection_fraction[heart$BP=="High BP"], 
     xlab = "Ejection Percentage",
     ylim = c(0,30))

hist(heart$ejection_fraction[heart$BP=="Not High BP"],
     xlab = "Ejection Percentage",
     ylim = c(0,100))

# Stand. dev. ejection fraction of high blood pressure groups 
tapply(X=heart$ejection_fraction, INDEX = heart$BP, FUN = sd)

#   High BP      Not High BP 
#   12.42291    11.53139 

# Check the variance in both groups high BP and not high BP groups 
var(heart$ejection_fraction[heart$BP == "High BP"])

# 154.3288

var(heart$ejection_fraction[heart$BP == "Not High BP"])

# 132.9729

# Because the variances are different we assume unequal variance

# Run two sample t-test using both groups (heart$ejection_fraction by heart$BP)

t.test(heart$ejection_fraction~heart$BP, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

# t = 0.41214, df = 200.16, p-value = 0.6807
# 95 percent confidence interval:
# -2.289808  3.499921

########### Interpretation
# Since there is a small t statistics, there is a large p-value. 
# Because the p-value is greater than 0.05, we fail to reject the null
# hypothesis. There is no association between the ejection fraction and 
# whether or not the patient has high blood pressure

###############################################################################

### Research Question 3: ANOVA ###
 

# View serum creatinine variable
View(heart$serum_creatinine)
summary(heart$serum_creatinine)

# Create a new variable for different creatinine levels
heart$creatinine_lvl = factor(NA, levels = c("low", "normal","high"))

# Re-assign values of the new creatinine levels 
heart$creatinine_lvl[heart$serum_creatinine < 0.84] = "low"
heart$creatinine_lvl[heart$serum_creatinine >= 0.84 & heart$serum_creatinine <= 1.21] = "normal"
heart$creatinine_lvl[heart$serum_creatinine >1.21] = "high"

################################### Plots #####################################

# Draw boxplots for serum_sodium in each creatinine level
boxplot(heart$serum_sodium ~ heart$creatinine_lvl,
        xlab = "Creatinine Level", 
        ylab = "Serum Sodium Level")

# View the means serum sodium level of each group 
tapply(heart$serum_sodium, heart$creatinine_lvl, mean)

# low   normal     high 
# 137.1429 137.6913 134.8020

# Create a histogram for sodium serum within each creatinine level 
hist(heart$serum_sodium[heart$creatinine_lvl=="low"],
     xlab = "Serum Sodium Level")
hist(heart$serum_sodium[heart$creatinine_lvl=="normal"],
     xlab = "Serum Sodium Level")
hist(heart$serum_sodium[heart$creatinine_lvl=="high"],
     xlab = "Serum Sodium Level")

##################################  Analysis  ##################################

# Conduct ANOVA
anova.sodium = aov(heart$serum_sodium ~ heart$creatinine_lvl)

# View results
summary(anova.sodium)


#                       Df  Sum Sq Mean Sq F value   Pr(>F)    
# heart$creatinine_lvl   2    518  259.10   14.52 9.69e-07 ***
# Residuals            296   5284   17.85

########### Interpretation
# Because the p=value is smaller than the alpha level, 0.05, we reject the null 
# hypothesis. We conclude that there is enough evidence to support the claim 
# that at least one group has a different average amount of serum sodium 
# in blood than other groups  

###############################################################################


### Research Question 4: Linear Regression ###

# View ejection fraction variable 
View(heart$ejection_fraction)
summary(heart$ejection_fraction)

# View age variable
View(heart$age)
summary(heart$age)

################################### Plots #####################################

# We graph the two vectors of data on boxplots to check for outliers
boxplot(heart$ejection_fraction,
        ylab = "Percentage of blood leaving the heart",
        main = "Boxplot for Ejection Fraction")
boxplot(heart$age,
        ylab = "Age",
        main = "Boxplot for Age")


#By this method we find that there are two data points in ejection_fraction, 
#at 70 and 80, that are outliers of the data

##################################  Analysis  ##################################

# Plotting the age of the patients against their ejection fraction with a 
# scatterplot
# scatterplot
plot(heart$age, heart$ejection_fraction, main = "Ejection Fraction and Age", 
     xlab = "Age", ylab = "Ejection Fraction")


# Finding the coefficients of the best-fit linear model for the data as well as
# finding the F statistic
linMod <- lm(formula = heart$ejection_fraction ~ heart$age)

# View summary of linear regression 
summary(linMod)

# Coefficients:
#           Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 34.44603    3.57196   9.643   <2e-16 ***
# heart$age    0.05980    0.05763   1.038      0.3 

# Adding the slope line to our scatterplot
abline(34.44603, 0.05980)

# Finding the 95% confidence interval for the coefficients
confint(linMod)

# 2.5 %     97.5 %
# (Intercept) 27.41646134 41.4755920
# heart$age   -0.05361753  0.1732083

########### Interpretation
# Because the p-value, 0.3, is higher than 0.05, we fail to reject the null 
# hypothesis. We conclude that there is not enough evidence to prove an 
# association between ejection fraction and the age of the patient.






