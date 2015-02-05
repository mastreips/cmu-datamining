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
