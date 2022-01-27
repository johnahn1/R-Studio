#  Group 4 
# Dolores Juarez
# QTM 100
# Lab Instructor : Sena Agrezo 
# Lab Practice 6 
# Oct 1 2020

####################### QUESTION 1 ##########################

# Set working directory & import the 'abductees' data 
setwd("C:/Users/Dolores Juarez/Documents/QTM 100/lab_datasets/")
abductees <- read.csv("abductees.csv")

# See summary of data 
str(abductees)
summary(abductees)


###################### QUESTION 2 ###########################

# Create a new variable (Age) that calculates the age
#  of the survey respondent at the time of the survey 

abductees$Age <- 90 - abductees$yearbir


###################### Question 3 ##########################

# Create a new variable 'edulevel' to classify education 
#   status, where 12 years of education and less is 
#   'high school', 13-16 year is 'college education" and 
#   more than 16 is 'more than college'

abductees$EduLevel <- factor(NA,levels = c("high school","college","more than college"))

abductees$EduLevel[abductees$educat<=12]<-"high school"
abductees$EduLevel[abductees$educat>12 & abductees$educat<= 16] <-"college"
abductees$EduLevel[abductees$educat>16]<-"more than college"

# Check the recoding fo the variable 
summary(abductees$EduLevel)
table(abductees$educat, abductees$EduLevel)



######################  QUESTION 4 ############################

# Create a table to see the categories for marital status
table(abductees$marstat)
# Possible reponses for martital status include:
#   divorced, married, separated, single 

# Create a new marital status variable that classifies 
#   individuals as either "Married" or "Other"

abductees$Married <- factor(NA, levels =c("Married", "Other"))

# Re-Assign values of the new martial status variable where 
#   "Married" is classified as "Married" and where "Divorced, 
#   "Separated", and "Single" will be classified as "Other"
abductees$Married[abductees$marstat== "Married"] <- "Married"
abductees$Married[abductees$marstat== "Divorced" | abductees$marstat == "Single" | abductees$marstat == "Separated"] <- "Other"

# Check the re-coding of "Married"
table(abductees$marstat, abductees$Married)


######################## QUESTION 5 ##########################

# Create a table to see the different categories for a 
#   respondents experience 

table(abductees$abdfeel)

# Groups include: "About equally positive and negative" 
#   "Entirely negative", "Entirely positive", "Mostly 
#   negative", "Mostly positive"

                    ### PART A ###

# Create a side-by-side box plot that compares the age 
#   distribution among the 5 groups of experiences

boxplot(abductees$Age ~ abductees$abdfeel)

                    ### ANSWER ### 
# The respondents who had an entirely positive experience
#   have the highest median age, while the the respondents 
#   who had an entirely entirely negative and a mostly 
#   negative experience had the lowest lowest median age. 
#   The youngest respondents had an entirely negative 
#   experience. If this were on a scale, where 0 is entirely 
#   negative and 5 being entirely positive, we would somewhat
#   see a trend as age increases the experience is more 
#   positive, but this trend line would not be very linear, 
#   as the ranges for the ages seem to be quite large




                    ### PART B ###

# Create a new variable for experiences from abduction
abductees$ExpNew <- factor(NA, levels =c("Entirely negative", "Mostly negative", "About equally positive and negative", "Mostly positive" , "Entirely positive"))

# Order the factor levels in an ordinal fashion 
# Eg the worst experience to the best 
abductees$ExpNew[abductees$abdfeel== "Entirely negative"] <- "Entirely negative"
abductees$ExpNew[abductees$abdfeel== "Mostly negative"] <- "Mostly negative"
abductees$ExpNew[abductees$abdfeel== "About equally positive and negative"] <- "About equally positive and negative"
abductees$ExpNew[abductees$abdfeel== "Mostly positive"] <- "Mostly positive"
abductees$ExpNew[abductees$abdfeel== "Entirely positive"] <- "Entirely positive"

# Create a side-to-side box plot comparing the age 
#   distributions among the newly ordered factor levels 
boxplot(abductees$Age ~ abductees$ExpNew)

                    ### AnSWER ### 

# Now the tend seems to be much more clearer. As the age of
#   the respondents increases, the experiences becomes more
#   positive.


####################### QUESTION 6 ##########################

                    ### PART A ###

# Create a new variable that puts all females together
abductees$MorF[abductees$sex== "Female" | abductees$sex == "female"] <- "Female"
abductees$MorF[abductees$sex== "male"] <- "Male"

# See the number of individuals within each sex using the new
#   variable created 
table(abductees$MorF)

# Create a table to compare the average ages within each sex
tapply(X = abductees$Age, INDEX = abductees$MorF, FUN = mean)
boxplot(abductees$Age ~ abductees$MorF)




                      ### PART B ###

# Create a table that shows the average number of abductions
#   among the married/ other factor levels 
tapply(X = abductees$abdtimes, INDEX = abductees$Married, FUN = mean,na.rm = FALSE)

                      ### ANSWER ###

# The average number of abductions within the individuals 
#   classified as "other" is 3.25 times 




                    ### PART C ### 

# create a table of the individuals within each factor level 
#   for education 
table(abductees$EduLevel)

                    ### ANSWER ###

# 29 individuals have no more than a college level education 





                    ### PART D ###  

                    ### ANSWER ###

# Age age increases, the abduction experience improves (gets
#   more positive)


