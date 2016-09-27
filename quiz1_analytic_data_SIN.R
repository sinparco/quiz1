#Install Packages
##install.packages("tidyverse",dep=T)
##install.packages("psych",dep=T)
##install.packages("apaTables", dep=T) 

#Load Packages
library(tidyverse)
library(psych)
library(haven)

#Load Data
raw_data <- read_csv(file="lab_quiz_week2_data.csv")

#Fix Data
raw_data <- read_csv(file="lab_quiz_week2_data.csv",na=c("","NA","-999"))

#Labelling Data
categorical_variables <- select(raw_data, univ, prog_year)
categorical_variables$univ <- as.factor(categorical_variables$univ)
categorical_variables$prog_year <- as.factor(categorical_variables$prog_year)
levels(categorical_variables$univ) <- list("Waterloo"=1, "Guelph"=2)
levels(categorical_variables$prog_year) <- list("First Year"=1, "Second Year"=2, "Third Year"=3, "Fourth Year"=4, "Grad School"=5)

#Creating Item Scales
positive_affect_items <- select (raw_data, PA1, PA2, PA3, PA4, PA5)
depression_items <- select (raw_data, D1, D2, D3, D4, D5)
program_satisfaction_items <- select (raw_data, PS1, PS2, PS3, PS4, PS5)

#Descriptive Analysis
#psych::describe(depression_items)

#Fixing Bad Values
is_bad_value <- positive_affect_items<1 | positive_affect_items>7
positive_affect_items[is_bad_value] <- NA
is_bad_value <- depression_items<1 | depression_items>4
depression_items[is_bad_value] <- NA
is_bad_value <- program_satisfaction_items<1 | program_satisfaction_items>6
program_satisfaction_items[is_bad_value] <- NA

#Fixing Inverted Items
positive_affect_items <- mutate(positive_affect_items, SE1=8-PA1)
depression_items <- mutate(depression_items, D4=5-D4)
depression_items <- mutate(depression_items, D5=5-D5)
program_satisfaction_items <- mutate(program_satisfaction_items, PS1=7-PS1)
program_satisfaction_items <- mutate(program_satisfaction_items, PS2=7-PS2)

#Obtaining Scale Scores
pos_affect <- psych::alpha(as.data.frame(positive_affect_items), check.keys=FALSE)$scores
dep <- psych::alpha(as.data.frame(depression_items), check.keys=FALSE)$scores
prog_sat <- psych::alpha(as.data.frame(program_satisfaction_items), check.keys=FALSE)$scores

#Combine into analytic_data
analytic_data <- cbind(categorical_variables, raw_data$age, pos_affect, dep, prog_sat)

#Saving .RData, CSV, .SAV 
save(analytic_data,file="quiz1_analytic_data_SIN")
write_csv(analytic_data,path="quiz1_analytic_data_SIN")
