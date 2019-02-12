###############################################################################
## Filename: EMPIWIFO Uebung 4
## Created on: 6 March 2018 
## Last modified: 
## Note: Solution
###############################################################################
install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

rm(list=ls( ))

load("Exercise 4/eren_ozbeklik.RData")



# 1) Konstruieren Sie zwei Histogramme, die die Verteilung des durchschnittlichen
# wochentlichen Lohn im ersten Jahr nach dem Experiment (also im vierten Jahr) fur
# die Treatment- und Kontrollgruppe darstellen. Kann anhand dieser Histogramme
# etwas uber den ffekt des Programmes ausgesagt werden?


# Histogramm der data für Treatment und Kontrollgruppe

ggplot(aes(x = EARNY4), data = data[!is.na(data$assignment),]) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~assignment)


par(mfrow=c(1,2))

hist(data$EARNY4[data$assignment==0], breaks = 50)
hist(data$EARNY4[data$assignment==1], breaks = 50)

par(mfrow=c(1,1))



# 2) Gibt es relevante Unterschiede im durchschnittlichen wochentlichen Lohn im vierten
# Jahr zwischen Individuen unterschiedlicher Ethnie und unterschiedlichen Geschlechts?
# Wenn ja, was konnte das für den e t der Programmteilnahme bedeuten?


# "agegroup" und "race_eth" zu Factors umwandeln

data$female <- as.factor(data$female)
levels(data$female) <- c("male", "female")

data$agegroup <- as.factor(data$agegroup)
levels(data$agegroup) <- c("agegroup1", "agegroup2", "agegroup3")

data$race_eth <- as.factor(data$race_eth)
levels(data$race_eth) <- c("white", "black", "hispanic", "indian")


# Deskriptive Statistiken zu EARNY4, abhÃ¤ngig ob Person mÃ¤nnlich oder weiblich ist, oder bestimmter Ethnie angehÃ¶rt

summarise(group_by(data, race_eth), mean = mean(EARNY4, na.rm = TRUE))

summarise(group_by(data, agegroup), mean = mean(EARNY4, na.rm = TRUE))

summarise(group_by(data, female), mean = mean(EARNY4, na.rm = TRUE))




# 3) Generieren Sie folgende Variablen, welche zu einem spateren Zeitpunkt benotigt
# werden. Als Erstes generieren Sie eine Dummyvariable fur jede unterschiedliche
# Ethnie, und zudem auch eine Dummyvariable fur jede unterschiedliche Alterskategorie
# ( 1) zwischen 16 und 17 Jahre alt, 2) zwischen 18 und 19 Jahre alt, und 3)
# zwischen 20 und 24 Jahre alt).

# Don't do that.




# 4) Welcher Prozentsatz der Treatmentgruppe hat tatsachlich an dem Programm teilgenommen?
# Welcher Prozentsatz der Kontrollgruppe hat im vierten Jahr mehr als
# 10,000 $ pro Jahr verdient?

# Tabellen
tab_participation <- count(data, assignment, participation)

mutate(group_by(tab_participation, assignment), perc = n/sum(n))


# More as 10000 $

count(data, EARN4)

data <- mutate(data, EARN4 = floor(EARN4))

mutate(group_by(count(data, assignment, EARN4 == 1), assignment), perc = n/sum(n))




# 5) Schatzen Sie obiges Modell und interpretieren Sie alle Koefizienten. Diskutieren
# Sie anhand der p-Werte, ob die Koefizienten statistisch signikant sind. Benutzen
# Sie STATA um zu testen, ob die Koefizienten von white, hispanic und indian
# gemeinsam statistisch signifikant sind.

# Regression

# Check Levels

levels(data$female)
levels(data$agegroup)
levels(data$race_eth)



# Re-level
data$race_eth <- relevel(data$race_eth,"black") # Schwarze als Referenzgruppe verwenden
levels(data$race_eth)

data_reg <- lm(EARNY4 ~ assignment + female + agegroup + race_eth, data) 
summary(data_reg) # # Beachte, dass das erste Level eines Factors als Refernzgruppe verwendet wird




# F-Test, gemeinsame statistische Signifikanz der Variable race_eth

data_reg_restricted <- lm(EARNY4 ~ assignment + female + agegroup, data) 

anova(data_reg_restricted, data_reg)



# Log-Level-Modell schÃ¤tzen

data_reg_log <- lm(log(EARNY4) ~ assignment + female + agegroup + race_eth, data)
# Funktioniert nicht, da diverse Personen als EARNY4 0 angegeben haben, und log(0) nicht definiert ist
# die Beobachtungen mit EARNY4=0 mÃ¼ssten gedroppt werden

data_reg_log <- lm(log(EARNY4) ~ assignment + female + agegroup + race_eth, data[data$EARNY4 > 0,]) # 
summary(data_reg_log) # So schÃ¤tzbar, aber Koeffizienten vermutlich verzerrt, da auf abhÃ¤ngige Variable selektiert wird ->

