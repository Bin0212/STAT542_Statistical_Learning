library(gbm)
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
myfit1 = gbm(Y ~ . , data = mydata[-test.id, ], 
             distribution = "gaussian", 
             n.trees = 100,
             shrinkage = 1, 
             interaction.depth = 3, 
             bag.fraction = 1,
             cv.folds = 5)
myfit1
# black is training error, green is cv error
gbm.perf(myfit1)
opt.size = gbm.perf(myfit1)

# test data
size = 1:myfit1$n.trees
test.err = rep(0, length(size))
for(i in 1:length(size)){
  y.pred = predict(myfit1, mydata[test.id, ], n.trees = size[i])
  test.err[i] = sum((mydata$Y[test.id] - y.pred)^2)
}    
plot(test.err, type = "n")
lines(size, test.err, lwd = 2)
abline(v = opt.size, lwd = 2, lty = 2, col = "blue")

# another gbm (tuning parameters)
myfit2 = gbm(Y ~ . , data = mydata[-test.id, ], 
             distribution = "gaussian", 
             n.trees = 1000,
             shrinkage = 0.1, 
             interaction.depth = 3, 
             bag.fraction = 0.5,
             cv.folds = 5)
gbm.perf(myfit2)
gbm.perf(myfit2)

# test data
size = 1:myfit2$n.trees
test.err = rep(0, length(size))
for(i in 1:length(size)){
  y.pred = predict(myfit2, mydata[test.id, ], n.trees = size[i])
  test.err[i] = sum((mydata$Y[test.id] - y.pred)^2)
}    
plot(test.err, type = "n")
lines(size, test.err, lwd = 2)
abline(v = opt.size, lwd = 2, lty = 2, col = "blue")
opt.size = gbm.perf(myfit2)

# variable importance
par(mfrow=c(1, 2))
summary(myfit1, cBars = 10,
        method = relative.influence, 
        las = 2)
summary(myfit1, cBars = 10,
        method = permutation.test.gbm, 
        las = 2)
par(mfrow = c(1,1))
