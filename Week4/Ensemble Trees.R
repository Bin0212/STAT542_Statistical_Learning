library(randomForest)
library(MASS)
library(glmnet)

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
mydata = myData
n = nrow(mydata)
ntest = round(n * 0.3)
set.seed(1234)
test.id = sample(1:n, ntest)
# Fit a random forest
rfModel = randomForest(Y ~ ., data = mydata[-test.id, ],
                       importance = T, ntree=400); 
names(rfModel)
## the default value for mtry is p/3 for regression
## p = ncol(mydata) - 1 = 15
## mtry = 14/3 = 5
rfModel$mtry

# test error 
yhat.test = predict(rfModel, mydata[test.id, ])
sum((mydata$Y[test.id] - yhat.test)^2)/length(test.id)

# two training error
yhat.train = predict(rfModel, mydata[-test.id, ])
sum((mydata$Y[-test.id] - yhat.train) ^2)/(n - ntest)

sum((mydata$Y[-test.id] - rfModel$predicted) ^2)/(n - ntest)
rfModel$oob.times[1:5]

length(rfModel$oob)
## oob.times --> ntree * exp(-1) = ntree * 0.368
rfModel$ntree * exp(-1)
mean(rfModel$oob.times)

# The plot function for randomForest
tmp = rfModel$mse
par(mfrow=c(1, 2))
plot(rfModel)
plot(c(0, rfModel$ntree), range(tmp), type="n",
     xlab = "Number of trees", ylab="Error")
lines(tmp)
par(mfrow=c(1, 1))

# variable importance
rfModel$importance
importance(rfModel, scale = F)
cbind(importance(rfModel, scale = TRUE), 
      importance(rfModel, scale = F)[,1]/rfModel$importanceSD)
par(mfrow = c(1,2))
varImpPlot(rfModel, type=1)
varImpPlot(rfModel, type=2)
