#loading the file 
getwd()
setwd("C:/Users/ASUS X571LI/Desktop/QTM LAB/Datasets/")
yrbss2013 <- read.csv("yrbss2013.csv", header = T)


# FIGURE 1
  #Code for basic plot
  plot(x= yrbss2013$height_m, y= yrbss2013$weight_kg)

  #Code for custom plot + labels 
  plot(x = yrbss2013$height_m,
     y = yrbss2013$weight_kg,
    
     xlab = "Height (m)",
     ylab = "Weight (kg)")
  #Result: 

# FACT 1
  #Mean of the respondent weight
  mean(yrbss2013$weight_kg)
  
  #Result: The mean of the respondent weight is "66.626 kg"
  
# FACT 2
  #Mean of the respondent age
  mean(yrbss2013$age)
  
  #Result: The mean of the respondent age is "15.8" years old  

# FACT 3 

  