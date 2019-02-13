mypackages = c("leaps", "glmnet")   
tmp = setdiff(mypackages, rownames(installed.packages())) 
if (length(tmp) > 0) install.packages(tmp)

library(leaps)  # regsubsets
library(glmnet)  # glmnet for lasso

# set random seed in case you want to reproduce the result
set.seed(542)  
n = 1000
x1 = rnorm(n)
x1 = rnorm(n)
x2 = rnorm(n)
e = rnorm(n)
x3 = 2/3 * x1 + 2/3 * x2 + 1/3 * e
epsilon = rnorm(n)
beta = c(2, 3)
y = beta[1] * x1 + beta[2] * x2 + epsilon
myData = data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

## Use AIC/BIC to select the best sub-model
IC = regsubsets(y ~ ., myData, method = "exhaustive")
sumIC = summary(IC)
sumIC$bic
sumIC
msize = apply(sumIC$which, 1, sum)
AIC = n * log(sumIC$rss/n) + 2 * msize
BIC = n * log(sumIC$rss/n) + log(n) * msize
AIC; BIC # both will pick the 2 variables cases which is the correct model

## Use LASSO
# Next, we use LASSO with lambda.min and lambda.1se to check if it can select the correct model.

mylasso.cv = cv.glmnet(as.matrix(myData[, c('x1', 'x2', 'x3')]), as.vector(myData$y))
plot(mylasso.cv)
coef(mylasso.cv, s = 'lambda.1se')
coef(mylasso.cv, s = 'lambda.min')
# Neither approach selects the correct model. The following code provides two 
# path plots of LASSO: the x-coordinate is the L1-norm of the coefficients in 
# the “norm” plot and the x-coorindate is log-lambda in the “lambda” plot. You 
# can see that X3 won’t be dropped out of the model unless log-lambda is going 
# to −∞ (or equivalently λ→0). Thus it is impossible for LASSO to select the 
# correct model.
mylasso = glmnet(as.matrix(myData[, c('x1', 'x2', 'x3')]), as.vector(myData$y))
par(mfrow = c(2, 1))
plot(mylasso, label=TRUE, xvar = "norm")
plot(mylasso, label=TRUE, xvar = "lambda")

## Compare prediction performance
# Next we can compare the corresponding prediction error on a test set.

N = 1000
mytestData = matrix(0, N, 4)
colnames(mytestData) = c("x1", "x2", "x3", "y")
mytestData = as.data.frame(mytestData)

mytestData$x1 = rnorm(N)
mytestData$x2 = rnorm(N)
mytestData$x3 = 2/3 * x1 + 2/3 * x2 + 1/3 * rnorm(N)
mytestData$y = beta[1] * mytestData$x1 + beta[2] * mytestData$x2 + rnorm(N)

tmp = predict(mylasso.cv, s="lambda.min", 
              newx=data.matrix(mytestData)[, -4])
mean((mytestData$y - tmp)^2)

tmp = predict(mylasso.cv, s="lambda.1se", 
              newx=data.matrix(mytestData)[, -4])
mean((mytestData$y - tmp)^2)

myfit.full = lm(y ~ ., myData)
tmp = predict(myfit.full, newdata=mytestData)
mean((mytestData$y - tmp)^2)

myfit.AIC = lm(y ~ x1 + x2, myData)
tmp = predict(myfit.AIC, newdata=mytestData)
mean((mytestData$y - tmp)^2)

