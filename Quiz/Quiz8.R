library(ISLR)
library(pROC)
library(MASS)
library(glmnet)

data = Caravan
dim(data)

test <- data[1:1000,]
train <- data[-c(1:1000),]

test$Purchase = as.numeric(test$Purchase) - 1
train$Purchase = as.numeric(train$Purchase) - 1

#4.1
model1 <- glm(Purchase~., data=train, family=binomial)
summary(model1)

pred1 <- predict.glm(model1, newdata = test[,-86], type="response")
mypred1 = (pred1>0.25)
table(mypred1, test[,86])

roc(test[,86], pred1)

#4.2
fit1 = glm(Purchase~., data=train, family=binomial)
fit2 = glm(Purchase~1, data=train,family=binomial)
step.aic = stepAIC(fit2, direction = "forward", scope=list(upper=fit1,lower=fit2), trace=0)

pred2 <- predict.glm(step.aic, newdata = test[,-86], type="response")
mypred2 = (pred2>0.25)
table(mypred2, test[,86])

roc(test[,86], pred2)

#4.3
fit1 = glm(Purchase~., data=train, family=binomial)
fit2 = glm(Purchase~1, data=train, family=binomial)
n=dim(train)[1]
step.bic = stepAIC(fit2, direction = "forward", scope=list(upper=fit1,lower=fit2), 
                   trace=0, k=log(n))

pred3 <- predict.glm(step.bic, newdata = test[,-86], type="response")
mypred3 = (pred3>0.25)
table(mypred3, test[,86])

roc(test[,86], pred3)

#4.4
model4 <- glmnet(as.matrix(train[,-86]), train[,86], family='binomial', alpha=1, lambda = 0.004)

pred4 <- predict(model4, newx = as.matrix(test[,-86]), type="response")
mypred4 = (pred4>0.25)
table(mypred4, test[,86])

roc(test[,86], pred4)
