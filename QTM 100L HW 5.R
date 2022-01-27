# Dolores Juarez
# Group 4
# QTM 100 Lab 
# Instructor : Sena Agrazo 
# Lab Practice 5 

##### Part One: Sampling Distribution for Proportions #######

####################### QUESTION 1 #########################

# Import yrbss data set 
setwd("C:/Users/Dolores Juarez/Documents/QTM 100/lab_datasets")
yrbss <- read.csv("yrbss2013.csv", header = TRUE )

# PART (a)
summary(yrbss$gender)

# Create a table that shows the number of people per gender
#   and store the table as a variable 
gender <- yrbss$gender
table(gender)
table_gender <- table(gender)

# See the proportion of the gender 
prop.table(table_gender)

# Create a table that shows number of students who have
#   been sad for 2 weeks or more in a row 
sad <- yrbss$sad
table(sad)
table_sad <- table(sad)

# See the proportion of the students who have been
#   sad for 2 weeks or more in a row 
prop.table(table_sad)

####################### ANSWER ###########################

# Proportion For the "Gender" variable 
#     Male = 0.496
#     Female = 0.504

# Proportion of students who have been sad for 2 or more
#   weeks in a row  
#   No = 0.706
#   Yes = 0.294






##################### QUESTION 2 ###########################

# Part (a)

      #### Data for the Gender variable ###### 

# Take 100 sample of size 10 from the population and
#   calculate the proportions female to male of each sample 

# Create an empty matrix of 100 rows and two columns
gender_100 <- matrix(rep(NA, 100), nrow= 100, ncol = 2)

# Create the loop
for(i in 1:100){
  sample_gender <- sample(gender, 10)
  gender_100[i,] <- prop.table(table(sample_gender))
}

# See the column means of the new matrix 
colMeans(gender_100)



          #### Data for Sad Variable ######

# Take 100 samples of size 10 from the population and
#   calculate the proportions of students who have been sad
#   for two or more weeks in a row

# Create an empty matrix of 100 rows and two columns
sad_100 <- matrix(rep(NA, 100), nrow= 100, ncol = 2)

# Create loop for sad vector
for(i in 1:100){
  sample_sad <- sample(sad, 10)
  sad_100[i,] <- prop.table(table(sample_sad))
}

# See the average proportions of the new matrix 
colMeans(sad_100)

################# PART A ANSWER ###########################

# Average Proportions for each categories of100 samples

# Gender : 
# Female = 0.49
# Male = 0.51

# Whether or not student where sad 
#   No = 0.687
#   Yes = 0.313



# Part (b) 

        #### Data for the Gender variable ###### 

# Take 500 sample of size 10 from the population and
#   calculate the proportions female to male of each sample 

# Create an empty matrix of 500 rows and two columns
gender_500 <- matrix(rep(NA, 500), nrow= 500, ncol = 2)

# Prints the i-th value
for(i in 1:500){
  sample_gender <- sample(gender, 10)
  gender_500[i,] <- prop.table(table(sample_gender))
}

# See the summary of the new matrix 
colMeans(gender_500)



            #### Data for Sad Variable ######

# Take 500 samples of size 10 from the population and
#   calculate the proportions of whether or not each of the 
#   students were sad  

# Create an empty matrix of 500 rows and two columns
sad_500 <- matrix(rep(NA, 500), nrow= 500, ncol = 2)

# Create a for loop for the samples 
for(i in 1:500){
  sample_sad <- sample(sad, 10)
  sad_500[i,] <- prop.table(table(sample_sad))
}

# See the column proportions of the new matrix 
colMeans(sad_500)

################# ANSWER PART B #############################

# Average Proportions for each categories of 500 samples

# Gender :
# Female = 0.51
# Male = 0.49

# Whether or not students were sad 
#   No = 0.699
#   Yes = 0.336





# Part (c)

        #### Data for the Gender variable ###### 

# Take 5,000 samples of size 10 from the population and
#   calculate the proportions female to male of each sample 

# Create an empty matrix of 100 rows and two columns
gender_5000 <- matrix(rep(NA, 5000), nrow= 5000, ncol = 2)

# Create the loop
for(i in 1:5000){
  sample_gender <- sample(gender, 10)
  gender_5000[i,] <- prop.table(table(sample_gender))
}

# See the column means of the new matrix 
colMeans(gender_100)


            #### Data  FOR Sad Variable ####

# Take 5000 samples of size 10 from the population and
#   calculate the proportions of whether or not each of the 
#   students were sad 

# Create an empty matrix of 5000 rows and two columns
sad_5000 <- matrix(rep(NA, 5000), nrow= 5000, ncol = 2)

# Create the for loop
for(i in 1:5000){
  sample_sad <- sample(sad, 10)
  sad_5000[i,] <- prop.table(table(sample_sad))
}

# See the column means of the new matrix 
colMeans(sad_5000)

################# ANSWER PART C #############################

# Average Proportions for each categories of 500 samples

# Gender :
# Female = 0.505
# Male = 0.495

# Whether or not student had ridden with drunk driver
#   No = 0.705
#   Yes = 0.325



# Part (d)
################## ANSWER PART D #############################

# Increasing the number of samples makes the average
#   proportions from each loop closer to the ones real 
#   observed proportion of the population



#################### QUESTION 3 ##########################

# PART A 

#Create a matrix that has 5,000 rows and 2 columns 
gender_5kx100 <- matrix(rep(NA, 5000), nrow = 5000, ncol = 2)

# Create the for loop of 5000 samples of size 100
for(i in 1:5000){
  sample_gender100 <- sample(gender, 100)
  gender_5kx100[i,] <- prop.table(table(sample_gender100))
}

# View the column means
colMeans(gender_5kx100)

################### ANSWER PART A ##########################

# Average proportions for gender 
# Female = 0.504
# Male = 0.496




# PART B

# Create a matrix that has 5,000 rows and 2 columns 
gender_5kx200 <- matrix(rep(NA, 5000), nrow = 5000, ncol = 2)

# Create the for loop of 5000 samples of size 100
for(i in 1:5000){
  sample_gender200 <- sample(gender, 200)
  gender_5kx200[i,] <- prop.table(table(sample_gender200))
}

# View the column means
colMeans(gender_5kx200)

################### ANSWER PART B ##########################

# Average proportions for gender 
# Female = 0.503
# Male = 0.497


# PART C

# Create a matrix that has 5,000 rows and 2 columns 
gender_5kx500 <- matrix(rep(NA, 5000), nrow = 5000, ncol = 2)

# Create the for loop of 5000 samples of size 100
for(i in 1:5000){
  sample_gender500 <- sample(gender, 500)
  gender_5kx500[i,] <- prop.table(table(sample_gender500))
}

# View the column means
colMeans(gender_5kx500)

################### ANSWER PART C ##########################

# Average proportions for gender 
# Female = 0.504
# Male = 0.496







