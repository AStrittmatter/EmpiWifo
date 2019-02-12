###############################################################################
## Filename: EMPIWIFO Uebung 5
## Created on: 22 January 2019 
## Last modified: 
## Note: Solution
###############################################################################
install.packages(c("dplyr", "ggplot2", "lmtest", "sandwich"))

library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)

rm(list=ls( ))

load("Exercise 5/WAGE2.RData")


# Descriptives
summary(data$wage)


# Generieren Sie eine neue Variable hwage, die den Stundenlohn misst 
# (hwage = wage/(hours*4)). Wieso ist es wichtig, Stundenlohne anstatt
# monatliche Einkommen zu betrachten? 
# Generieren Sie auch die neue Variable lhwage=ln(hwage).

data <- mutate(data, hwage = wage / (4*hours))
data$hwage <- data$wage / (4*data$hours)
  
# Stundenlohnvariable (auch logarithmiert) generieren
data <- mutate(data, lhwage = log(hwage))
data$lhwage <- log(data$hwage)



# 2. Berechnen Sie den Mittelwert und die Standardabweichung des Stundenlohns 
# jeweils fur black und non-black. Vergleich von Stundenlohn zwischen Schwarzen
# und Nicht-Schwarzen

# Mean
data_mean <- group_by(data, black)
summarise(data_mean, 
          Mean_wage = mean(hwage, na.rm = TRUE), 
          Sd_wage = sd(hwage, na.rm = TRUE))

mean(data$hwage[data$black == 1])
mean(data$hwage[data$black == 0])

sd(data$hwage[data$black == 1])
sd(data$hwage[data$black == 0])

ggplot(data_mean, aes(x = hwage)) +
  geom_histogram() +
  facet_wrap(. ~ factor(black, labels = c("Non black", "Black"))) +
  geom_vline(aes(xintercept = mean(hwage)), color = "red") 
  


# 3. Regressieren Sie hwage auf eine Konstante und black.
wage_reg <- lm(hwage ~ black, data = data)
summary(wage_reg)  



# 4. Generieren Sie eine Dummyvariable nonblack=1-black. Regressieren Sie hwage auf
# eine Konstante, black und nonblack. Warum wird automatisch eine Dummyvariable
# weggelassen?

# Dummy-Variable Trap
data$nonblack = 1 - data$black

# Regressieren Sie hwage auf eine Konstante
wage_reg_trap <- lm(hwage ~ black + nonblack, data = data)  
summary(wage_reg_trap) 

# "Non-Black" wird automatisch omitted da die Summe von Black und Non-Black perfekt 
# multikollinear mit der Konstante ist (1 not defined because of singularities)


# 5. Regressieren Sie hwage auf black und nonblack ohne eine Konstante. Wie konnen
# Sie die Regressionskoeffizienten von black und nonblack interpretieren? Welcher
# Zusammenhang besteht zwischen den Regressionskoeffizienten in dieser Teilaufgabe
# und Teilaufgabe 3.a?
wage_reg_noconst <- lm(hwage ~ -1 + black + nonblack, data = data)
summary(wage_reg_noconst) 
# Wird ohne Konstante spezifiziert, können Black und Nonblack implementiert werden





# 6. Regressieren Sie lhwage auf alle verfugbaren erklarenden Variablen
# (d.h. alle Variablen in den Daten ausser wage, lwage, hours, hwage, nonblack).

data_small <- select(data, -c(wage, lwage, hours, nonblack, hwage))

wage_reg_multi <- lm(lhwage ~ . , data = data_small) 
summary(wage_reg_multi)    



# c. Wie gross ist der vorhergesagte Stundenlohn von unverheirateten Afroamerikanern,
# die in einer landlichen Gegend im Suden leben, wenn alle anderen Regressoren
# gleich ihrem Durchschnittwert sind?
average_guy <- data_small[complete.cases(data_small), ]
average_guy <- summarise_all(average_guy, funs(mean(., na.rm = TRUE))) 
average_guy <- mutate(average_guy,
                      married = 0,
                      black = 1,
                      south = 1,
                      urban = 0)

exp(predict(wage_reg_multi, average_guy))



# d. Wie andert sich der vorhergesagte Lohn, wenn dieselbe Person
# verheiratet ist?
average_married_guy <- mutate(average_guy, married = 1)

exp(predict(wage_reg_multi, average_married_guy))





# 7. Sie mochten wissen, ob die Bildungsrendite fur Afroamerikaner im Vergleich zum
# Rest unterschiedlich ist.
interacted_model <- lm(lhwage ~ educ*black + ., data = data_small)
summary(interacted_model)






# 8. Sie mochten ausserdem wissen, ob auch die Koffizienten der ubrigen Regressoren
# fur Afroamerikaner und den Rest unterschiedlich sind.
fully_interacted_model <- lm(lhwage ~  black*., data = data_small)
summary(fully_interacted_model)

coeftest(fully_interacted_model, vcov. = sandwich)


# Testen Sie, ob es Modellheterogenitat bezuglich Hautfarbe gibt.
# Chow-Test auf Effektheterogenität

# Ohne Interaktionsterme mit black und ohne Gruppendummy black
restricted_model <- lm(lhwage ~ . -black, data = data_small)
summary(restricted_model)

anova(fully_interacted_model, restricted_model)
