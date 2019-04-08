library(MASS)
library(glmnet)
library(rpart)
library(randomForest)

myData = Boston
names(myData)[14] = "Y"
iLog = c(1, 3, 5, 6, 8, 9, 10, 14);
myData[, iLog] = log(myData[, iLog]);
myData[, 2] = myData[, 2] / 10;
myData[, 7] = myData[, 7]^2.5 / 10^4
myData[, 11] = exp(0.4 * myData[, 11]) / 1000;
myData[, 12] = myData[, 12] / 100;
myData[, 13] = sqrt(myData[, 13]);

X = as.matrix(myData[, -14])
y = myData$Y

myfit = rpart(Y~., data = myData)
printcp(myfit)

myrf = randomForest(Y~., data = myData)
myrf$predicted[1]

printcp(myfit)
prune(myfit, cp=0.015)
