###############################################################################
## Filename: EMPIWIFO Uebung 2
## Created on: 6 March 2018 
## Last modified: 
## Note: Solution
###############################################################################
install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

rm(list=ls( ))

load("Exercise 2/BWGHT.RData")

# Set working directory	
# setwd("O:/EmpiWifo/Uebung2/Data")


#===========================    Exercise  1)   =================================

# 1) Show the distribution of the birth weight and the education of the mothers in the
# sample and comment on it.

summary(data$bwght)
hist(data$bwght)

hist(data$bwght, freq=TRUE, breaks=30, xlab="Birth weight", main="Distribution of birth weight", 
     col="lightgreen", xlim=c(0,300),  ylim=c(0, 350))

hist(data$bwght, freq=FALSE, breaks=30, xlab="Birth weight", main="Distribution of birth weight", 
     col="pink", xlim=c(0,300))

curve(dnorm(x, mean=mean(data$bwght), sd=sd(data$bwght)), add=TRUE, col="darkred", lwd=2) 


ggplot(aes(x = bwght), data = data) +
  geom_histogram(bins = 30)



# Distribution of motheduc

summary(data$motheduc)

ggplot(aes(x = motheduc), data = data) +
  geom_histogram(binwidth = 1)

hist(data$motheduc, freq=TRUE, main="Distribution of mother education", xlim=c(0,20))


#===========================    Exercise  2)   =================================
# Mean bwght when mother is not smoking
mean(data$bwght)
mean(data$bwght[data$cigs != 0])
mean(data$bwght[data$cigs == 0]) 


summarise(group_by(data, cigs == 0),
          mean_bwght = mean(bwght, na.rm = TRUE)) 


# Mean bwght when mother smokes one pack per day
mean(data$bwght[data$cigs == 20])



# ===========================    Exercise  3)    ================================
# Code missing values of fatheduc
summary(data$fatheduc)

library(Hmisc)
Hmisc::describe(data$fatheduc)
unique(data$fatheduc)
table(data$fatheduc)

data$fatheduc[data$fatheduc == -1] <- NA


# Dplyr
mutate(data, fatheduc = ifelse(fatheduc == -1, NA, fatheduc))



#===========================    Exercise  5)    ================================
#Correlation between cigs and faminc
cor(data$cigs, data$faminc)




# ===========================    Exercise  6)    ================================
# Run regression of bwght on cigs
ols1 <- lm(bwght ~ cigs, data = data)
summary(ols1)
plot(ols1)



# Run regression of bwght on cigs and faminc
ols2 <- lm(bwght ~ cigs + faminc, data = data)
summary(ols2)
plot(ols2)




# ===========================    Exercise  8)    ================================
# Run regression of bwght on cigs and faminc
ols3 <- lm(bwght ~ cigs + faminc + packs, data = data)
summary(ols3)

# ==============================    END   =======================================

