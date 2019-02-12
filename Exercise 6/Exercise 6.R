###############################################################################
## Filename: EMPIWIFO Uebung 5
## Created on: 22 January 2019 
## Last modified: 
## Note: Solution
###############################################################################
install.packages(c("dplyr", "AER"))

library(dplyr)
library(AER)

rm(list=ls( ))

load("Exercise 6/CARD.RData")


# 4. Testen Sie, ob das Instrument nearc4 mit der zu instrumentierenden Variable educ
# korreliert ist. Um den Test durchzufuhren, regressieren Sie educ auf eine Konstante
# und nearc4 und uberprufen Sie den Koeffizienten von nearc4. Warum ist dieser
# Koeffizient von nearc4 informativ bezuglich der zu untersuchenden Korrelation?

cor(data$educ, data$nearc4)

# Relevanz des Instruments nearc4 testen
relevanz <- lm(educ ~ nearc4, data)
summary(relevanz)


# 6. Fur einen Teil der Stichprobe ist ein IQ-Testergebnis verfugbar. Regressieren Sie
# IQ auf eine Konstante und nearc4, um herauszufinden, ob der durchschnittliche IQ
# damit variiert, ob die Person in der Nahe eines 4-Jahres-College aufgewachsen ist.
# Warum ist dies nutzlich und welche Schlusse ziehen Sie daraus?


# Korreliert nearc4 mit einer Grösse im Fehlerterm (z.B. IQ?)
iqnearc4 <- lm (IQ ~ nearc4, data)
summary(iqnearc4)  


# Regionen als Kontrollvariablen, korreliert nearc4 immer noch signifikant mit IQ?

iqregion <- lm(IQ ~ nearc4+ smsa66 + 
                    reg661 + reg662+ reg663+ reg664+ reg665+ reg666+ reg667+ reg668+ reg669, 
               data)
summary(iqregion) # Dummy-Variable Trap - ein Regionen Dummy muss aussenvor gelassen werden


data_small <- select(data, matches("^reg"), IQ, nearc4, smsa66)

iqregion2 <- lm(IQ ~ ., data_small)
summary(iqregion2)  


# 10. Uberprufen Sie, ob das Instrument nearc4 mit der zu instrumentierenden Variable
# educ weiterhin korreliert ist, wenn wir alle anderen erklarenden Variablen in die
# log(wage)-Gleichung einbeziehen. Warum ist es wichtig, dies zu testen?
# Erneute Prüfung der Relevanz des Instrument nearc4

educregion <- lm(educ ~ nearc4 + smsa66 + exper + expersq + black + smsa + south + 
                   reg662+ reg663+ reg664+ reg665+ reg666+ reg667+ reg668+ reg669, 
                 data)
summary(educregion)


# 11. Schatzen sie Cards log(wage)-Gleichung, indem Sie erst OLS und dann IV mit nearc4
# als Instrument verwenden. Interpretieren Sie die Ergebnisse.

ols <- lm(lwage ~ educ + smsa66 + + exper + expersq + black + smsa + south + 
            reg662+ reg663+ reg664+ reg665+ reg666+ reg667+ reg668+ reg669, 
          data)
summary(ols) 
waldtest(ols)


iv <- ivreg(lwage ~ educ + smsa66 + exper + expersq + black + smsa + south +  
               reg662+ reg663+ reg664+ reg665+ reg666+ reg667+ reg668+ reg669 
             | . - educ + nearc4 , data = data)

summary(iv)

