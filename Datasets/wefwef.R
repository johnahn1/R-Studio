getwd()
setwd("c:/Users/ASUS X571LI/Desktop/QTM LAB/Datasets/")

babies = read.table("babies.txt", header = T)

str(babies)

summary(babies)


babies$parityF = factor(babies$parity, labels = c("first born", "otherwise"))
babies$smokeF = factor(babies$smoke, labels = c("not now", "yes now"))



str(babies)
summary(babies)

summary(babies$bwt)
mean(babies$bwt)
sd(babies$bwt)

tapply(X = babies$bwt, INDEX = babies$smokeF, FUN = mean)
?tapply


hist(babies$bwt)
boxplot(babies$bwt)

boxplot(babies$bwt ~ babies$smokeF)

plot(babies$gestation, babies$bwt)



#####categorical 

table(babies$smokeF)

smk.tab = table(babies$smokeF)

addmargins(smk.tab) 
prop.table(smk.tab)

table(babies$smokeF, babies$parityF)

smk.par.tab = table(babies$smokeF, babies$parityF)
addmargins(smk.par.tab)
prop.table(smk.par.tab)

prop.table(smk.par.tab, margin = 1)

barplot(smk.tab)
