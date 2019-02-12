###############################################################################
## Filename: EMPIWIFO Uebung 3
## Created on: 6 March 2018 
## Last modified: 
## Note: Solution
###############################################################################
install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

rm(list=ls( ))

load("Exercise 3/VOTE1.RData")


#===========================    Exercise  1)   =================================

# 1) Generate two scatter plots that show the relationship between voteA on the vertical
# axis and expendA, respectively expendB, on the horizontal axis. Make sure that
# your plots also contain a line showing the linear relationship between the variables.
# Interpret its slope.


# Descriptives
describe(data)


# Scatter-Plots mit linearem Fit
plot(voteA ~ expendA, data)
slr <- lm(voteA ~ expendA, data)
abline(coef(slr), col = "red")


ggplot(aes(x = expendA, y = voteA), data = data) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  xlab("\n Expenditure A in 1000$") + 
  ylab("Voting outcome candidate A in % \n") 


plot(voteA ~ expendB, data)
slr2 <- lm(voteA ~ expendB, data)
abline(coef(slr2))

ggplot(aes(x = expendB, y = voteA), data = data) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  xlab("\n Expenditure B in 1000$") + 
  ylab("Voting outcome candidate A in % \n") 



# 2) What is the mean voting outcome in percent of candidate A, given that his party
# received more than 50% of the votes in the recent presidential election and less
# than 50%? Do you see the results you would have expected?
# Mittelwertsvergleiche

mean(data$voteA[data$prtystrA > 50])
mean(data$voteA[data$prtystrA <= 50])  




# 3) Generate the following three variables which we will need at a later stage: first, 
# generate the log of expendA, expendB, and second a variable that contains the 
# difference between the expenditures of candidate B and the expenditures of candidate A.

# Log-Variablen generieren

data$lexpendA <- log(data$expendA)
data$lexpendB <- log(data$expendB)

data$expendiff <- data$expendA - data$expendB



# 4) Run the regression model stated above and interpret each coefficient as well as the
# intercept. Discuss whether the coefficients are significant using the p-values. Write
# down the respective null hypothesis and the formula for the t-statistic to show how
# STATA computes the statistical significance.


# Regression
Vote_reg <- lm(voteA ~ expendA + expendB + prtystrA, data)
summary(Vote_reg)




# 5) In terms of parameters, state the null hypothesis that a 1000$ increase in A's
# expenditures is oset by a 1000$ increase in B's expenditures. Write down the
# respective test-statistic. Can you use the regression results to test the stated null
# hypothesis? Use STATA to nally test the outlined hypothesis.


# Spezifischer T-Test, man teste ob Koeffizient von expend A und Koeffizient B zusammen 0 ergeben
# Nullhypothese: beta_1 + beta_2 = 0
# Alternativhypothese: beta_1 + beta_2 != 0 (zweiseitiger Test)
# Nullhypothese in Nullform Ã¼berfÃ¼hren (ist sie bereits): beta_1 + beta_2 = 0 = theta
# UrsprÃ¼ngliches Modell: y = beta_0 + beta_1*x1 + beta_2*x2 + beta_3*x3 + u
# Modell umformen: y = beta_0 + (beta_1 + beta_2)*x1 + beta_2*(x2 - x1) + beta_3*x3 + u
# Nun erkennt man, dass (beta_1 + beta_2) = theta
# Also y = beta_0 + theta*x1 + beta_2*(x2-x1) + beta_3*x3 + u
# Um nun unsere Nullhypothese zu testen, kÃ¶nnen wir einfach theta gegen null testen

install.packages("car")
library(car)

linearHypothesis(Vote_reg, "expendB = - expendA")



# Alternative way
Vote_reg_trick <- lm(voteA ~ expendA + expendiff + prtystrA, data)
summary(Vote_reg_trick)






# 6) Level-Log-Modell

Vote_reg_log <- lm(voteA ~lexpendA + lexpendB + prtystrA, data)
summary(Vote_reg_log)




# 7) Test if in the model estimated in the last exercise, log(expendA) and log(expendB)
# are jointly significant. Write down the respective null hypothesis and the respective
# test-statistic. Use STATA to test the hypothesis of joint significance and discuss
# if we can reject the null hypothesis.

# F-Test, Koeffizient von expendA = 0 und Koeffizient von expendB = 0


# By hand:
# restringiertes Modell
Vote_reg_restricted <- lm(voteA ~ prtystrA, data)



# SST, SSE und SSR (Sum of Squares Total, Sum of Squares Explained (Model), Sum of Squares Residuals) kÃ¶nnen von Hand berechnet werden
ssr_restricted <- sum(Vote_reg_restricted$residuals^2) # SSR: sum[uhat_i] = sum[(y_i - yhat_i)]
ssr_unrestricted <- sum(Vote_reg$residuals^2) # SSR: sum[uhat_i] = sum[(y_i - yhat_i)]


# q : number of restrictions
q <- 2


# n : number of observations
N <- nobs(Vote_reg_restricted)


# k : number of variables in unrestricted model (degrees of freedom in the model)
k <- 3


# F-test
Ftest <- ((ssr_restricted - ssr_unrestricted)/q) / (ssr_unrestricted / (N-k-1))



# Alternative: use the ANOVA function
anova(Vote_reg, Vote_reg_restricted)
