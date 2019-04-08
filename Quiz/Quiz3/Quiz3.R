library("ElemStatLearn")
library("MASS")
library("glmnet")
data("prostate")
names(prostate)  
## "lpsa" the 2nd last var is the response variable
## "train" is the indicator for training data
prostate$train
table(prostate$train)  # 67 training vs 30 testing

## Fit a linear regression model to predict lpsa
traindata = prostate[prostate$train==TRUE,]
testdata = prostate[prostate$train==FALSE,]

## Remove the "train" indicator column
traindata = within(traindata, rm(train))
testdata = within(testdata, rm(train))

myfit = lm(lpsa ~ . , data=traindata)
myfit  ## display the estimated LS coefficients
summary(myfit)  ## more output

mypredict = predict(myfit, newdata=testdata)

## mean squared error on the training and test sets. 
sum((traindata$lpsa - myfit$fitted)^2)/nrow(traindata)  
sum((testdata$lpsa - mypredict)^2)/nrow(testdata)     

# Level-wise search
#why b and rs 's rss are different??
library(leaps)
n = dim(traindata)[1];
p = 8
b = regsubsets(lpsa ~ ., data=traindata, nvmax = p)
rs = summary(b)
msize = 1:p;
par(mfrow=c(1,2))
Aic = n*log(rs$rss/n) + 2*msize;
Bic = n*log(rs$rss/n) + msize*log(n);
cbind(rs$which[which.min(Aic),], rs$which[which.min(Bic), ])
myfit.aic = lm(lpsa ~ .-gleason , data=traindata)
myfit.bic = lm(lpsa ~ lcavol + lweight , data=traindata)
mypredict.aic = predict(myfit.aic, newdata=testdata)
mypredict.bic = predict(myfit.bic, newdata=testdata)
sum((testdata$lpsa - mypredict.aic)^2)
sum((testdata$lpsa - mypredict.bic)^2)


X = as.matrix(traindata[,-9])
Y = as.matrix(traindata[9])
X_new = as.matrix(testdata[, -9])
Y_new = as.matrix(testdata[9])
mylasso = glmnet(X, Y, alpha = 1, lambda = c(0.5, 0.1, 0.01))
Ytest.pred = predict(mylasso, newx = X_new)
sum((Y_new - Ytest.pred[,3])^2)/dim(testdata)[1] # averaged MSE on the test set
