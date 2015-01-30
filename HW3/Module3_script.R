library(ggplot2)
library(lattice)
library(rpart)
library(party)
library(partykit)
library(vcd)
library(vcdExtra)
library(arules)
library(plyr)
library(dplyr)
library(C50)
library(AppliedPredictiveModeling)
library(caret)
library(e1071)
library(reldist)
library(gains)

setwd("~/Google Drive/Tepper/Data Mining/Module 3") #mac

data <- read.csv("hmeq.csv", header=TRUE, stringsAsFactors=TRUE)

#look at your data set characteristics 
str(data)
table(is.na(data)) #missing values

summary(data) #more granular detail on missing values

#missing values :  http://www.statmethods.net/input/missingdata.html

#pairwise deletion 
newdata <-na.omit(data)
table(is.na(newdata))
summary(newdata)

#impute mean

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

#should be in a for loop
data_new <- ddply(data, ~JOB,transform, 
                  DEBTINC = impute.mean(DEBTINC))
data_new <- ddply(data_new, ~JOB,transform, 
                  DEROG = impute.mean(DEROG))
data_new <- ddply(data_new, ~JOB,transform, 
                  MORTDUE = impute.mean(MORTDUE))
data_new <- ddply(data_new, ~JOB,transform, 
                  VALUE = impute.mean(VALUE))
data_new <- ddply(data_new, ~JOB,transform, 
                  YOJ = impute.mean(YOJ))
data_new <- ddply(data_new, ~JOB,transform, 
                  DELINQ = impute.mean(DELINQ))
data_new <- ddply(data_new, ~JOB,transform, 
                  CLAGE = impute.mean(CLAGE))
data_new <- ddply(data_new, ~JOB,transform, 
                  NINQ = impute.mean(NINQ))
data_new <- ddply(data_new, ~JOB,transform, 
                  CLNO = impute.mean(CLNO))

summary(data_new) ## compare to summary(data)

#split the data 67% train/33%test

data_new$BAD <- as.factor(data_new$BAD)  #set to factor for target
sample_idx <- sample(nrow(data_new), nrow(data_new)*0.67)
data_train <- data_new[sample_idx, ]
data_test <- data_new[-sample_idx, ]


#CART Classification Tree
rpartFull <- rpart(BAD ~ ., data=data_train)
rparty <- as.party(rpartFull)
plot(rparty)

#Test Set Results
rpartPred <- predict(rpartFull, data_test, type= "class")
confusionMatrix(rpartPred, data_test$BAD)
gain1 <- gains(as.numeric(data_test$BAD), as.numeric(rpartPred), groups=2,  conf="t")
print(gain1)
plot(gain1)

#Score new data
data2 <- read.csv("score.csv")
rpartPred2 <- predict(rpartFull, data2, type="class")
rpartProb <- predict(rpartFull, data2, type="prob")
pred_data <- cbind(data2, 'BAD'=predict(rpartFull, data2, type="class")) #add predicted HIT
pred_data <- cbind(pred_data, 'PROB'=rpartProb) #add probability

#order records by highest to lowest confidence. 
ord_data <- arrange(pred_data, desc(PROB.0))

#response chart
plot((1-quantile(ord_data$PROB.1, probs=seq(0,1,0.01))), type="l")
