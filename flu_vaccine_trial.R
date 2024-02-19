install.packages("tidyverse")
library

install.packages("readxl")
#install.packages("devtools")
install.packages("caret")
install.packages('fastDummies')
devtools::install_github("tidyverse/readxl")
install.packages(nnet)
install.packages("scales")
install.packages("memsic")
install.packages("MASS")

library(readxl)
library(nnet)
library(lmtest)
library('fastDummies')
library(scales)
library(memsic)
library(MASS)

#library(ISLR)
library(dplyr)
library(tidyr)


response <- c('ASmall', 'BModerate', 'CLarge','ASmall', 'BModerate', 'CLarge')
treatment <- c('Placebo', 'Placebo', 'Placebo', 'Vaccine', 'Vaccine', 'Vaccine')
frequency <- c(25, 8, 5, 6, 18, 11)

trial <- data.frame(Treatment = treatment, Response = response, Freq = frequency)

ressat.trial<-glm(Freq~Treatment*Response,family=poisson,
                     data=trial)
resadd.trial<-glm(Freq~Treatment + Response,family=poisson,
                     data=trial)
resmin.trial<-glm(Freq~1, family=poisson,data=trial)


print(resadd.trial$fitted.values)
fit_p <- resadd.trial$fitted.values
print(resadd.trial$family$dev.resids(mu = 1, wt = 1, y = 1))
print(resadd.trial$deviance)

#ANSWER: A)


numeric_trial <- sapply(trial, as.numeric)


trial.expanded <- trial[rep(row.names(trial), trial$Freq), 1:2]
trial.table <- table(trial.expanded)

chisq.test(trial.table)
qchisq(0.95, df=3)

#The conventional Chi-squared modelling is significant compared to 
#chi(3) distribution

logLik(resadd.trial)
logLik(ressat.trial)
 
C = 2*(-logLik(resadd.trial)-logLik((resmin.trial)))
print(C)

delta_D = 2*(-logLik(resadd.trial)-logLik((ressat.trial)))
print(delta_D)
predict(resadd.trial, type='response')

predictions <- predict(resadd.trial, type='response')



#ANSWER B)

#Pearson
#
residuals(ressat.trial, type = "pearson")

#PEARSON RESIDULS OF THE ADDITIVE MODEL(CORRESPONDING TO THE HYPOTHESIS OF HOMOGENIETY)
residuals(resadd.trial, type = "pearson")

residuals(resmin.trial, type = "pearson")

#Deviance
residuals(ressat.trial, type = "deviance")

#DEVIANCE RESIDULS OF THE ADDITIVE MODEL(CORRESPONDING TO THE HYPOTHESIS OF HOMOGENIETY)
residuals(resadd.trial, type = "deviance")

residuals(resmin.trial, type = "deviance")


#Pearson
sum(residuals(ressat.trial, type = "pearson")**2)

#CHI SQUARED OF THE ADDITIVE MODEL(CORRESPONDING TO THE HYPOTHESIS OF HOMOGENIETY)
sum(residuals(resadd.trial, type = "pearson")**2)


sum(residuals(resmin.trial, type = "pearson")**2)

#Deviance
sum(residuals(ressat.trial, type = "deviance")**2)

#DEVIANCE OF THE ADDITIVE MODEL(CORRESPONDING TO THE HYPOTHESIS OF HOMOGENIETY)
sum(residuals(resadd.trial, type = "deviance")**2)


sum(residuals(resmin.trial, type = "deviance")**2)


#greatest contribution
#Pearson
max(residuals(ressat.trial, type = "pearson"))

#MAXIMUM CONTRIBUTION TO CHI SQUARED IN THE ADDITIVE MODEL(CORRESPONDING TO THE HYPOTHESIS OF HOMOGENIETY)
max(residuals(resadd.trial, type = "pearson"))

max(residuals(resmin.trial, type = "pearson"))

#Deviance
max(residuals(ressat.trial, type = "deviance"))


#MAXIMUM CONTRIBUTION TO DEVIANCE IN THE AADDITIVE MODEL(CORRESPONDING TO THE HYPOTHESIS OF HOMOGENIETY)
max(residuals(resadd.trial, type = "deviance"))


max(residuals(resmin.trial, type = "deviance"))


#Interpretation
#1- pchisq(17.648, 0.95) = 0.000000 therefore this chi-squareed is very significant compared to X^2(2) distribution
#the pseudo R^2 of  0.1055417 (df=4) is somewhat of a good fit
# With a max of 3.6792023 for the Chi-squared and a nax if  3.215918 for the Deviance residuals
# the cell small Placibo contributes the most to the X^2 and Deviance

fit_p=c(fitted.values(res.britdoc))
pearsonresid<-(britdoc$deaths-fit_p)/sqrt(fit_p)


#########################################################################

trial$Treatment <- ifelse(trial$Treatment == 'Placebo', 1, 2)

trial$Response <- ifelse(trial$Response == 'ASmall', 1,
                         ifelse(trial$Response == 'BModerate', 2, 3))

resord.trial <- polr(factor(Freq)  ~ Treatment + Response, data = trial.table)


summary(resord.trial)



#chi.squared
print(chisq.test(trial.table))



 
