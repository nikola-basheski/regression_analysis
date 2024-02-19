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


df  <- read_excel('/home/nikola/Documents/Housing Data.xlsx')
mytable <- ftable(xtabs(~Housing+Contact_with_other_residents+Satisfaction, data = df))



#ANSWER A)
total <- mytable[,1] + mytable[,2] + mytable[, 3]
total_col <- mytable[1,] + mytable[2,] + mytable[3, ]

percentage_high <- (mytable[,1])/total
percentage_medium <-(mytable[, 2])/total
percentage_low <- (mytable[,3])/total

make_str <- function(list_str, list_percentage){
  new_list <- c()
  temp_strs <- c()
  for(i in 1: length(list_str)){
    temp_str <- label_percent()(list_percentage)
    new_list <- c(new_list, paste(as.character(list_str[i]), temp_str[i]))
  }
  
  return(new_list)
}


high_col <- make_str(mytable[,1], percentage_high)
medium_col <- make_str(mytable[,1], percentage_medium)
low_col <- make_str(mytable[,1], percentage_low)

mytable_df <- mytable
mytable_df[,1] <- high_col
mytable_df[,2]  <- medium_col
mytable_df[,3] <- low_col

mytable_df



#######################################################################################################
#ANSWER B)

minimal_model <- multinom(Satisfaction ~ 1, data = mytable, weights = Freq)
model <- multinom(Satisfaction ~ factor(Housing)+factor(Contact_with_other_residents), data = mytable, weights = Freq)

#model1 <- multinom(Satisfaction ~ factor(Housing)+factor(Contact_with_other_residents), data = newtable, weights = Freq)

newtable <- data.frame(mytable)
original_newtable <- data.frame(mytable)

get_all_params<-function(actual_model){
  #colnames(High       Low    Medium
  freq_high <- c(as.array(as.data.frame(predict(actual_model, type='probs'))$High[1:6]))
  freq_low <- c(as.array(as.data.frame(predict(actual_model, type='probs'))$Low[1:6]))
  freq_medium <- c(as.array(as.data.frame(predict(actual_model, type='probs'))$Medium[1:6]))
  
  probas <- c(freq_high, freq_low, freq_low)
  
  #new_order <- c(1, 3, 5, 2, 4, 6)
  mt_list <- split(newtable, f = list(newtable$Housing, newtable$Contact_with_other_residents))
  sums_list <- sapply(mt_list, function(x) sum(x$Freq, na.rm = TRUE))
  #print(sums_list)
  #sums_list <- sums_list[new_order]
  # 
  
  freqs <- c(freq_high*sums_list, freq_low*sums_list, freq_medium*sums_list)
  newmatrix <- newtable
  newmatrix$Probabilities <- probas
  newmatrix$Frequencies <- freqs
  resids <- (newmatrix$Freq-newmatrix$Frequencies)/sqrt(newmatrix$Frequencies)
  newmatrix$Residuals <- resids
  stdresids <- resids/sqrt((1/(length(resids)-1))*(sum(resids^2)))
  newmatrix$Standard_residuals <- stdresids
  chisq_model <- sum(resids^2)
  print(chisq_model)
  return(newmatrix)
}

params <- get_all_params(model)
print(params)
print(get_all_params(model))


lrtest(model, minimal_model)
C <- 2*(logLik(model)-logLik(minimal_model))
print(C)

#R^2
rsquared<-(logLik(minimal_model)-logLik(model))/(logLik(minimal_model))
rsquared

#AIC
AIC <- -2*(logLik(model))+16
AIC


#Odds Ratio
or <- exp(coef(model))
print(or)


#Confidence Intervals
ci <- confint(model, level = 0.95)
print(ci)


sum_residuals <- sum(params$Residuals^2)
print(sum_residuals)
print(qchisq(.95, df = 6))
#C=43.3973 is very significant compared to chi(6) = 12.59159
#p value is  1-pchisq( 43.3973, 6) = 9.732137e-08,  which means there is very little evidence in favor of the null hypothesis
#the model performs well

#MAXIMAL MODEL

maximal_model <- multinom(Satisfaction ~ factor(Housing)+factor(Contact_with_other_residents)+Housing*Contact_with_other_residents, data = newtable, weights = Freq)
maximal_model_matrix <-get_all_params(maximal_model)
lrtest(model, maximal_model)
D <- 2*(logLik(maximal_model)-logLik(model))
print(D)



#R^2
rsquared<-(logLik(model)-logLik(maximal_model))/(logLik(model))
rsquared

#Odds Ratio
or <- exp(coef(maximal_model))
print(or)


#Confidence Intervals
ci <- confint(maximal_model, level = 0.95)
print(ci)


#AIC
AIC <- -2*(logLik(maximal_model))+24
AIC

sum_residuals <- sum(maximal_model_matrix$Residuals^2)
print(sum_residuals)


print(D)
print(qchisq(.95, df = 4))
#D = 6.893028 which is not significant compared to chi(4) = 9.487729
#D = 6.893028 and sum of residuals = 6.93233 are very similar; 
#when compared with the distribution χ^2(4) they suggest
#that the previous model provides a good description of the data




#YES
#C - ORDINAL MODEL should be appropriate since the classes of the target variable are ordered.
# Low < Medium < High

new_mytable <- newtable
new_mytable$Housing_numeric <- as.numeric(new_mytable$Housing)
new_mytable$Housing_covariate <- ifelse(new_mytable$Housing_numeric == 1, 2,
                                        ifelse(new_mytable$Housing_numeric == 2, 1, 3))

# 
# new_mytable$Housing_covariate <- ifelse(new_mytable$Housing_numeric == 1, 3,
#                                         ifelse(new_mytable$Housing_numeric == 2, 1, 2))
# 

new_mytable
new_mytable$Contact_with_other_residents <- as.numeric(new_mytable$Contact_with_other_residents)

new_mytable$Housing1 <- as.numeric(new_mytable$Housing)
new_mytable$Housing2 <- as.numeric(new_mytable$Housing)
new_mytable$Housing3 <- as.numeric(new_mytable$Housing)

new_mytable$Housing1 <- ifelse(new_mytable$Housing1 == 1, 1, 0)
new_mytable$Housing2 <- ifelse(new_mytable$Housing2 == 2, 1, 0)
new_mytable$Housing3 <- ifelse(new_mytable$Housing3 == 3, 1, 0)

new_mytable$Satisfaction <- as.numeric(new_mytable$Satisfaction)


parsimonious_model <- multinom(Satisfaction ~  Housing_covariate + Contact_with_other_residents, data = new_mytable, weights = Freq)
parsimonious_model

delta_D <- 2*(logLik(model) - logLik(model_parsimonious))
print(delta_D)


parsim_D <- 2*(logLik(parsimonious_model) - logLik(minimal_model))
print(parsim_D)

  
lrtest(parsimonious_model,model)
  

get_all_params1<-function(actual_model){
  #colnames(High       Low    Medium
  freq_high <- c(as.array(as.data.frame(predict(actual_model, type='probs'))[1:6,1]))
  freq_low <- c(as.array(as.data.frame(predict(actual_model, type='probs'))[1:6, 2]))
  freq_medium <- c(as.array(as.data.frame(predict(actual_model, type='probs'))[1:6, 3]))
  
  probas <- c(freq_high, freq_low, freq_low)
  
  # new_order <- c(1, 3, 5, 2, 4, 6)
  mt_list <- split(original_newtable, f = list(original_newtable[,1], original_newtable[,2]))
  sums_list <- sapply(mt_list, function(x) sum(x$Freq, na.rm = TRUE))
  # print(sums_list)
  # sums_list <- sums_list[new_order]
  
  
  freqs <- c(freq_high*sums_list, freq_low*sums_list, freq_medium*sums_list)
  newmatrix <- newtable
  newmatrix$Probabilities <- probas
  newmatrix$Frequencies <- freqs
  resids <- (newmatrix$Freq-newmatrix$Frequencies)/sqrt(newmatrix$Frequencies)
  stdresids <- resids/sqrt((1/(length(resids)-1))*(sum(resids^2)))
  newmatrix$Residuals <- resids
  newmatrix$Standard_residuals <- stdresids
  chisq_model <- sum(resids^2)
  print(chisq_model)
  return(list(newmatrix, stdresids))
}

parsimonious_matrix <- get_all_params1(parsimonious_model)[1]
residuals <- get_all_params1(parsimonious_model)[2]
parsimonious_matrix



#R^2
rsquared<-(logLik(minimal_model)-logLik(parsimonious_model))/(logLik(minimal_model))
rsquared

#Odds Ratio
or <- exp(coef(parsimonious_model))
print(or)


#Confidence Intervals
ci <- confint(parsimonious_model, level = 0.95)
print(ci)



#AIC
AIC <- -2*(logLik(parsimonious_model))+12
AIC

sum_residuals <- sum(parsimonious_model_matrix$Residuals^2)
print(sum_residuals)

print(delta_D)
print(qchisq(.95, df = 2))
print(1-pchisq(18.81128, 2))

#delta_D = 18.81128 which is significant compared to chi(2) = 5.991465
#so on the grounds of both significance and parsimony model describes the data well










#######################################################################################################################################



new_mytable$Satisfaction_numeric <- as.numeric(new_mytable$Satisfaction)
new_mytable$Satisfaction_covariate <- ifelse(new_mytable$Satisfaction_numeric == 1, 1,
                                         ifelse(new_mytable$Satisfaction_numeric  == 2, 3, 2 ))



ordinal_model <- polr(factor(Satisfaction_covariate) ~ factor(Contact_with_other_residents) + factor(Housing_covariate), data = new_mytable, weights = Freq)
o_d <- 2*(logLik(model)-logLik(ordinal_model))



ordinal_matrix <- get_all_params1(ordinal_model)[1]
residuals <- get_all_params1(ordinal_model)[2]
ordinal_matrix

ordinal_matrix <- data.frame(ordinal_matrix)
sum(ordinal_matrix$Residuals^2)

print(o_d)
print(1-pchisq(4.806059, 7))
print(qchisq(.95, df = 7))
#p-value = 0.683617 so we fail to reject the null hypothesis
#On grounds of significance this model performs badly
#MODEL



#C
#Well performing and most parsimonious model is the parsimonious multinomial model from C
#according to the standardized residuals the following is where the largest discrepancies are
#between the observed frequencies and expected frequencies estimated
#from the model. Assuming abs(Standardized residual) > 2

#PARSIMONIOUS MODEL
#3  Tower_block                         High         High  100     0.5074989   172.04211 -5.4924872
#4    Apartment                          Low         High  111     0.3809348    67.42545  5.3066561
#9  Tower_block                         High          Low   34     0.3072537   104.15901 -6.8744034
#10   Apartment                          Low          Low  130     0.3433092    60.76573  8.8816110
#13   Apartment                         High       Medium  116     0.3362780   150.65252 -2.8232326
#15 Tower_block                         High       Medium   47     0.3072537   104.15901 -5.6006219
#18 Tower_block                          Low       Medium   54     0.3294693    72.15378 -2.1371622

#ORDINAL
#10   Apartment                          Low          Low  130     0.2709461    85.88991  4.75956072         2.00433042

# 2, 8, 11, 14, 17
