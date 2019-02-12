#############################################################################
# Empirische Wirtschaftsforschung - PC Uebung 1
# FS 2019
# 28.01.2019
#############################################################################

# Packages installieren
install.packages(c("dplyr", "ggplot2", "Hmisc"))

# Packages laden
library(dplyr)
library(ggplot2)
library(Hmisc)

# Projekt erstellen
# Erstellen von einem Projekt mit den Namen "R_exercises" über 
# "File/New project/New Directory/...". 

# Daten in den Workspace laden (die Daten sind im Projekt)
load("Exercise 1/classize.RData")


# Data Manipulation mit R:
# Spalten aus einem Dataframe wählen:
data$id       # Spalten mit Namen auswählen
data[, 1]     # Mit Spaltennummer (data[Zeile, Spalten])
data[, "id"]  # mit Namen
data[, 1:4]   #
data[, c("id", "totexpk")]

select(data, id) # dplyr(data, Spalten)

# Zeilen aus einem Dataframe wählen:
data[1,]

# Eine Variable und eine Beobachtung aus einem Dataframe wählen:
data[1,1]

# Einen neuen Objekt erstellen
col <- data[,1] 

# Neue Variabel erstellen
data$new <- data$tscorek/100
mutate(data, new = tscorek/100) #dplyr
  


# Übung 1 - Classize -----------------------------------------------------------
#1) After setting the workig directory, open the data set and use the describe and
#browse command to get a better picture of the data set at hand. Call a list
#containing tscorek and classize for the first ten observations. Check how many
#observations are in total in the data set.

summary(data)

Hmisc::describe(data)

View(data)


head(data[, c("tscorek", "classize")], n = 10)

print(data[, c("tscorek", "classize")], n = 30)

nrow(data)



#2) Rename a variable of your choice. (Re)-label the variables classize, tscorek and
#boy. Furthermore create a value label for boy and assign it. Finally re-order the
#variables.
names(data)
names(data)[3]
names(data)[names(data) == "tscorek"] <- "testscore"


#Tidyverse
data <- 
  data %>% 
    dplyr::rename(testscore = tscorek)



#Label variables: example
label(data$totexpk) <- "some_label"

attr(data$totexpk, "other_label") <- "test"

Hmisc::describe(data$totexpk)
attributes(data$totexpk)

attr(data$boy, "label") <- "1 for boy, 0 for girl"
describe(data$boy)
attributes(data$boy)




#Order variables: put id as the first variable
data <- data[, c(8, 1:7)]

data <- data[, c("id", names(data)[!names(data)=="id"])]

variable_to_put_in_front <- c("id", "testscore")

new_order_of_names <- c(variable_to_put_in_front, names(data)
                        [!(names(data) %in% variable_to_put_in_front)])

new_order_of_names

data[, new_order_of_names]

data[new_order_of_names]




#3) Generate the variable girl, indicating whether the observed individual is female
#(1) or male (0) and drop it again. Generate a variable that contains the number
#of observed kids per school. Lastly, sort the data set by id and save it under a
#different name.

# Generieren Sie die Variable \texttt{girl}, welche angibt, ob es sich um ein 
# Mädchen (1) oder einen Knaben (0) handelt und löschen Sie diese wieder.
data$girl <- ifelse(data$boy == 0, 1, 0)
data$girl

data <- data %>% mutate(girl = ifelse(boy == 0, 1, 0)) # Dplyr

# Variabel löschen
data[, -9]
data$girl <- NULL
select(data, -girl) #dplyr


#Generate variable containing number of kids per school
data_grouped <- group_by(data, schidkn)
data <- mutate(data_grouped, schoolsize = n())
data <- ungroup(data)


#Sort by id
data <- arrange(data, id)



#4) Use the summary command to get first descriptive statistics. Get the
#summary statistics of the test scores per classize. Use the tabulate command on
#classize and classize & boy to see the frequency counts. Finally produce a
#table showing the mean of tscorek, boy and freelunk per class size.

summary(data)
Hmisc::describe(data)


#Summary statistics of the test scores per classize
summary_tscore <- summarise(group_by(data, classize) , 
                    mean = mean(testscore, na.rm = TRUE), 
                    sd = sd(testscore, na.rm = TRUE), 
                    N = n()) # applies funs per each groups since the data is grouped

summary_tscore 

ggplot(aes(x = classize, y = mean), data = summary_tscore) +
  geom_bar(stat = "identity") + 
  coord_cartesian(ylim=c(900,1000))
  
plot(summary_tscore$classize, summary_tscore$mean, type = "h")



# Use the tabulate command on classize and classize & boy to see the frequency counts. 
# Finally produce a table showing the mean of tscorek, boy and freelunk per class size.
tab_classize <- count(data, classize)
tab_classize_boy <- count(data, classize, boy)

mutate(tab_classize, pct = n/sum(n))
mutate(tab_classize_boy, pct = n/sum(n))

mean_table <- group_by(data, classize)
summarise(mean_table, 
          mean_testscore = mean(testscore, na.rm = TRUE), 
          mean_boy = mean(boy, na.rm = TRUE),
          mean_freelunk = mean(freelunk, na.rm = TRUE))

summarise_at(mean_table, vars(testscore, boy, freelunk),
          funs(mean = mean(., na.rm = TRUE)))




#You are now interested in estimating the effect of class size on test score. Thus, you 
#consider the following bivariate regression model,

# Regression
ols <- lm(testscore ~ classize, data, na.action = na.omit) #na.omit: deletes any row with missing value
summary(ols)

library(stargazer)
stargazer(ols)



#Fitted values und Residuen generieren
data$predicted <- predict(ols)

data$fitted <- ols$fitted.values #add residuals to the data frame (make sure no missing values)




#More with broom
library(broom)
ols %>% tidy()
ols %>% augment()
ols %>% glance()


scoreexp %>% 
  augment()  %>% 
  ggplot(aes(x = .resid, y = .fitted)) +
    geom_point()


# Plot
plot(testscore ~ classize, data)
abline(coef(scoreexp), col = "red")


