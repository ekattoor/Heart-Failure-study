 



############# Importing data set and libraries######################
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gmodels)
library(corrplot)
library(Hmisc)
library(sjPlot)


heart_failure <- read.csv("~/Desktop/Esther's course/heart_failure_clinical_records_dataset.csv")
View(heart_failure)
###################converting variables###############

heart_failure$anaemia = as.character(heart_failure$anaemia)
heart_failure$anaemia = as.integer(heart_failure$anaemia)
############## Basic Data Exploration####################
names(heart_failure)
str(heart_failure)
class(heart_failure)
class(heart_failure$age)
dim(heart_failure)


head(heart_failure,10)
tail(heart_failure,4)


############ summary statistics######################

summary(heart_failure)
describe(heart_failure)
median(heart_failure$platelets)
mean(heart_failure$high_blood_pressure)
sd(heart_failure$serum_creatinine)

################ excluding missing values ####################



colSums(is.na(heart_failure))
is.na(heart_failure)

sapply(heart_failure, mean, na.rm=TRUE)

#######################Grouping some variables and making Graphs  ########################

#Categorising  patients with different levels of ejection fraction into risk groups #######
heart_failure$ejection_fraction1[heart_failure$ejection_fraction <=25] <- "High Risk"
heart_failure$ejection_fraction1[heart_failure$ejection_fraction >25 & heart_failure$ejection_fraction <=35] <- "Moderate Risk"
heart_failure$ejection_fraction1[heart_failure$ejection_fraction >35 & heart_failure$ejection_fraction <=44] <- "Low Risk"
heart_failure$ejection_fraction1[heart_failure$ejection_fraction >44] <- "Normal"
ggplot(data = heart_failure) +geom_bar(mapping = aes(x = heart_failure$ejection_fraction1),fill="pink")

#########Surviving and Deceased patients gender wise

mytable1 <- table(heart_failure$ejection_fraction1)
mytable <- mytable1[order(mytable1,decreasing = TRUE)]
barplot(mytable, main = "Patients at risk", ylab = "Count", border = "dark blue",col ="green")

heart_failure$name = heart_failure$serum_creatinine +heart_failure$serum_sodium
heart_failure$gender[heart_failure$sex==0]<-"Female"
heart_failure$gender[heart_failure$sex==1] <-"Male"


heart_failure$status[heart_failure$DEATH_EVENT==0]<-"Surviving"
heart_failure$status[heart_failure$DEATH_EVENT==1] <-"Deceased"
heart_failure$Current_state<-paste(heart_failure$status,heart_failure$gender)
mytable2 <- table(heart_failure$Current_state)
barplot(mytable2, main = "Survival Analysis - Gender", ylab = "Count", border = "dark blue",col =c("pink","blue","pink","blue"))
sjPlot::tab_xtab(var.row = heart_failure$Current_state, var.col = heart_failure$ejection_fraction1, title = "Survival by Risk Level", show.row.prc = TRUE)

############
heart_failure$agegrp<-cut(heart_failure$age,c(14,24,34,44,54,64,74,101),labels=c("15-24","25-34","35-44","45-54","55-64","65-74","75+"))
ggplot(data = heart_failure) +geom_bar(mapping = aes(x = heart_failure$agegrp),fill="purple")

#Scatter Plot between age and serum_creatinine
ggplot(heart_failure, aes(x=heart_failure$age, y=heart_failure$serum_creatinine, color=heart_failure$age)) +geom_point()
help("geom_point")
##################Taking Crosstabs for identifying associations#####################

heart_failure$diabetes_new = factor(heart_failure$diabetes, 
                                    levels = c(0,1),
                                    labels = c("Non-diabetic", "Diabetic"))
heart_failure$deathevent_new = factor(heart_failure$DEATH_EVENT, 
                                    levels = c(0,1),
                                    labels = c("Not dead", "Dead"))

CrossTable(heart_failure$diabetes_new,heart_failure$deathevent_new)
