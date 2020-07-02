rm(list=ls(all=TRUE))


library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library("dplyr")
library(randomForest)

setwd("C:/Users/User/Desktop/data science project/finance/credit risk modelling")


#load data##

data<-read.csv(file="loan.csv",header=T,sep=",")
dim(data)
str(data)
summary(data)

#checking missin vaalue##
sum(is.na(data))

data=na.omit(data)



##########Exploratory data analysis###########
library(DataExplorer)
create_report(data)

#html file created#


library(esquisse)
esquisse::esquisser(data)

library(dplyr)
library(ggplot2)

data %>%
 filter(!(Gender %in% "")) %>%
 filter(!(Married %in% "")) %>%
 filter(!(Dependents %in% 
    "")) %>%
 filter(!(Self_Employed %in% "")) %>%
 ggplot() +
 aes(x = Loan_Amount_Term, fill = Gender) +
 geom_histogram(bins = 30L) +
 scale_fill_viridis_d(option = "inferno") +
 theme_dark()

data %>%
 filter(!(Gender %in% "")) %>%
 filter(!(Married %in% "")) %>%
 filter(!(Dependents %in% 
    "")) %>%
 filter(!(Self_Employed %in% "")) %>%
 ggplot() +
 aes(x = Self_Employed, group = Married, weight = ApplicantIncome) +
 geom_bar(fill = "#0c4c8a") +
 theme_dark()

#feature selection#

library(earth)
marsModel <- earth(Loan_Status ~ ., data=data) # build model
ev <- evimp(marsModel)
plot(ev)

data$Loan_ID=NULL



#splitdata

set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test




#buildmodel#

library(caret)
library(ada)

model = ada(Loan_Status~ ., iter = 20,data = train, loss="logistic")

pred = predict(model, test)

pred

confusionMatrix(test$Loan_Status,pred)

#81%#


#randomforest


library(randomForest)

model_rf <- randomForest(Loan_Status~ ., data= train, ntree=10,mtry = 5)

pred1=predict(model_rf,test)

confusionMatrix(test$Loan_Status,pred1)

#75%#


#glm##


mode2 = glm(Loan_Status~ ., data = train)

pred = predict(mode2, test)

pred

confusionMatrix(test$Loan_Status,pred)
#82%#
