# install.packages("reldist")
install.packages("ROCR")
library(ggplot2)
library(lattice)
library(rpart)
library(party)
# library(partykit)
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
setwd("~/Google Drive/Tepper/Data Mining/HW2")
# setwd("D:/Users/mstreips/Google Drive/Tepper/Data Mining/HW2") #laptop


data <- as.data.frame(read.table("hw2_q1_data.txt", sep=" ", header=TRUE, stringsAsFactors=TRUE))
write.table(data, file ="hw2table.csv", sep=",", col.names=TRUE, row.names=FALSE, 
            quote=FALSE)

frmla <- buys.computer ~ age+income+student+credit.rating
fit <-rpart(buys.computer ~ age+income+student+credit.rating, method="class", data=data, 
            control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=.8)

fit2 <- ctree(frmla, data = data, controls = ctree_control(minsplit=2, minbucket=1))
plot(fit2)


##GINA
data$buys.computer <- as.numeric(data$buys.computer)
p <- data$buys.computer
gini(p)
data$buys.computer <- as.factor(data$buys.computer)


#C5.0
oneTree <- C5.0.formula(formula = frmla, data=data)
summary(oneTree)

##Problem 2
data2 <- read.csv("salary-class.csv")
frmla2 <- INCOME ~ .
fit3 <-rpart(frmla2, method="class", data=data2)
plot(fit3, uniform=TRUE)
text(fit3, use.n=TRUE, all=TRUE, cex=.8)

##partition data
smp_size <- floor(0.6*nrow(data2))
set.seed(1234)
train_ind <- sample(seq_len(nrow(data2)), size=smp_size)

train <- data2[train_ind,]
test <- data2[-train_ind,]

##logistic regression
lr <-glm(formula = INCOME ~ ., family = "binomial", data = data2)
lr2 <-glm(formula = INCOME ~ ., family = "binomial", data = train)
