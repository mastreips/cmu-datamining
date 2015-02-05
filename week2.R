#training and predicting

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <-spam[inTrain,]
testing <-spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type~. , data=training, method="glm")
modelFit
summary(modelFit)
modelFit$finalModel

predictions <- predict(modelFit, newdata=testing)
predictions
table(predictions)
summary(predictions)
confusionMatrix(predictions, testing$type)

#data slicing

#crossvalidation

set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)
sapply(folds,length)

folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds, length)

tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(folds)
folds$train[[1]]

#plotting predictdors

library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)
featurePlot(x=training[,c("age", "education", "jobclass")], y=training$wage, plot="pairs")

qplot(age,wage,data=training)
qplot(age,wage,color=jobclass,data=training)

qq <- qplot(age, wage,color=education, data=training)
qq+ geom_smooth(method='lm', formulat=y~x)

library(Hmisc)
cutWage <- cut2(training$wage, g=3)
table(cutWage)

p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))
p1

library(gridExtra)
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol=2)

t1 <- table(cutWage, training$jobclass)
t1

prop.table(t1, 1)

qplot(wage, color=education, data=training, geom="density")

#basic processing
hist(training$capitalAve, main="", xlab="avg. captial run length")

#standardize - skewed variable
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS) #must use train data when standardizing testing data set

preObj <- preProcess(training[,-58], method=c("center","scale")) #-58 is the target attribute
trainCapAves <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAves)
sd(trainCapAves)

testCapAveS <- predict(preObj, testing[,-58])$captialAve #do scaling for testing using training pre
mean(testCapAveS)

set.seed(32343)
modelFit <- train(type~., data=training, preProcess=c("center","scale"), method="glm")
modelFit

#imputing data
set.seed(13343)

#make som values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA]<- NA

#impute and standardize
library(RANN)
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

#standardize true values
capAveTruth <- training$capitalAve
capAveTruth <-(capAveTruth-mean(capAveTruth))/sd(capAveTruth)

#covariate creation
library(kernlab); data(spam)
spam$capitalAveSq <- spam$capitalAve^2

table(training$jobclass)
dummies <- dummyVars(wage~jobclass, data=training)
head(predict(dummies, newdata=training))

nsv<- nearZeroVar(training,saveMetrics =TRUE)
nsv

library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis

#nonlinear splines
lm_non_linear <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm_non_linear, newdata=training), col="red", pch=19, cex=0.5)

#splines on test set
predict(bsBasis, age=testing$age)

#Principal Value Decompositiond
M<- abs(cor(training[,-58]))
diag(M) <- 0
which(M>0.8, arr.ind=T)

names(spam)[c(34,32)]
plot(spam[,34], spam[,32])
names(spam)[c(58)]

#principal components
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2]) #similar to summing and adding variable for data compression
prComp$rotation

#PCA for entire dataset transformed to get skewed data standardized
typeColor <- ((spam$type=="spam")*1+1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor, xlab="PC1", ylab="PC2")

#PCA in caret
preProc <- preProcess(log10(spam[,-58]+1), method="pca",pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

#preprocess testing
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type ~., method="glm", data=trainPC)
testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

#using PCA as part of train fn
modelFit <- train(training$type ~., method="glm", preProcess="pca", data=training) #easier
confusionMatrix(testing$type, predict(modelFit, testing))
