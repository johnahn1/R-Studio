##
## Name: John Ahn
## Group members: Dolores Juarez, Mitik Merchant, Nick Su 
##

## QTM 100
## Date: 9/10/2020
## Lab Practice 3

###Q1
# Import the dataset ADNI
getwd()
setwd("C:/Users/ASUS X571LI/Desktop/QTM LAB/Datasets/")
ADNI = read.table("ADNI.txt", header = T)

# Get the dimensions of the dataset to find the # of observations 
dim(ADNI)

#Answer: There are 276 observations 


###Q2
# Shows the structure of the dataset
str(ADNI)

#Answer: The data below are recorded on the fillable Word Document.
# Description   DX          AGE         APOE4         GENDER         MMSE         adas         WholeBrain 
# In reality    categorical quant.      categorical   categorical    quant.       quant.      quant.
# In R          factor      numerical   integer       factor         integer      numerical    integer


###Q3
# Creates a factor variable based on the origingal integer variable
ADNI$APOE4_F = factor(ADNI$APOE4, labels = c("no copies", "one copy", "two copies"))

# Shows the summary of the new variable, APOE4_F
summary(ADNI$APOE4_F)

# Answer: The least common APOE4 genetic variant is "two copies" of the ApoE4 allele

###Q4
#   The graph that is appropriate to visualize the data with one numerical variable for
#   its distribution is Histogram. However, boxplot could also be used.

# creates a histogram of the ages of patients
hist(ADNI$AGE, 
     xlab = "ages of patients",
     main = "Histogram of the ages of patients")


#Answer: The graph is about symmetrical with the mode between ages 70 and 75. 
#        It is also unimodal. The mean and median is most likely to be between 70 and 75 
#        because the graph is symmetric and has the most frequency between those values.

###Q5
# a) Since DX is categorical and MMSE is quantitative, boxplot is the appropriate option because 
#    boxplot is best to visualize numerical variables with different categories. 
#    Therefore, the appropriate option for DX and adas is boxplot also. 

# b) 
# creates boxplots for MMSE and DX
boxplot(ADNI$MMSE ~ ADNI$DX,
        xlab = "Alzheimer's Disease Diagnosis",
        ylab = "MMSE Score",
        main = "Boxplot of MMSE score based on diagnosis")
#description: Patients with normal cognitive function have the highest median score of MMSE and tend
#             to receive higher scores than those in other groups. 
#             Patients with mild cognitive impairment have the second highest median and tend to receive 
#             higher scores than those in AD group but lower scores than those in Normal group. 
#             Patients with AD show a significantly low median, which makes sense because lower scores in 
#             MMSE indicate more cognitive impairment.

# creates boxplots for adas and DX
boxplot(ADNI$adas ~ ADNI$DX,
        xlab = "Alzheimer's Disease Diagnosis",
        ylab = "AD Assessement Scale",
        main = "Boxplot of adas based on diagnosis")
#description: Patients with normal cognitive function have the lowest median score and tend
#             to receive lower scores than those in other groups. 
#             Patients with mild cognitive impairment have the second highest median and tend to receive 
#             higher scores than those in Normal group but lower scores than those in AD group. 
#             Patients with AD have the highest median and tend to receive higher scores than those in other groups,
#             which makes sense because larger scores indicate greater dysfunction. 


# c) Alzheimer's Disease Assessment Scale cognitive test identifies potential outliers. 
#    Potential outliers are found in AD group and Normal group that took adas. 

###Q6
# a)
# creates a new variable for the smaller scale
ADNI$WholeBrainNew = ADNI$WholeBrain/100000

# b) *****Codes below are for the fillable chart*****
# shows the summary of patients
summary(ADNI)
# shows the standard deviation of the overall age 
sd(ADNI$AGE)
# shows the summary of categorical value, DX. 
table(ADNI$DX)

##### AGE VS DX #####
# compares statistics for age and DX
tapply(X = ADNI$AGE, INDEX = ADNI$DX, FUN = mean)
tapply(X = ADNI$AGE, INDEX = ADNI$DX, FUN = sd)

##### GENDER VS DX #####
# compares statistics for gender and DX
table(ADNI$DX, ADNI$GENDER)
DX_Gender_tab = table(ADNI$DX, ADNI$GENDER)
# provides the proportions based on rows  
prop.table(DX_Gender_tab, margin = 1)

##### BRAIN VOLUME VS DX #####
# shows the mean and std.dv of Brain Volume based on DX
tapply(X = ADNI$WholeBrainNew, INDEX = ADNI$DX, FUN = mean)
tapply(X = ADNI$WholeBrainNew, INDEX = ADNI$DX, FUN = sd)

##### APOE4 VS DX #####
# creates a table for APOE4 and DX
table(ADNI$APOE4_F, ADNI$DX)
DX_APOE4_tab = table(ADNI$APOE4_F, ADNI$DX)
DX_APOE4_tab
# calculates the proportions of APOE4 based on DX
prop.table(DX_APOE4_tab, margin = 2)
# calculates the proportions of DX from the overall
summary(ADNI$APOE4_F)/276

#The answer


###Q7
# The answer for Q7 is also on the Word Document. 

# The ADNI study has ___276__ participants. The average age is __73.6+/-7.0__
# years and _55.4__% are male. The Alzheimer's group has a lower average brain
# volume than the Normal group ( (9.7 +/- 1.2 )x10^5 vs (10.4 +/- 1.0) x10^5  mm3).
# Patients with Alzheimer's diagnosis have a higher prevalence of two copies of
# the APOE4 allele compared to normal diagnosis patients (20.4% vs 4.3%).
