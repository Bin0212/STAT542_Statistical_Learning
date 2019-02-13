library(glmnet)  # glmnet for lasso
library(ggplot2)  # qplot
library(gridExtra)  # grid.arrange, 
library(MASS)

# Data preparation
myData = Boston
names(myData)[14] = "Y"
iLog = c( 1,3,5,6,8,9,10,14 );
myData[,iLog] = log( myData[,iLog] );
myData[,2] = myData[,2] / 10;
myData[,7] = myData[,7]^2.5 / 10^4
myData[,11] = exp( 0.4 * myData[,11] ) / 1000;
myData[,12] = myData[,12] / 100;
myData[,13] = sqrt( myData[,13] );

n = nrow(myData)
p = ncol(myData) - 1
X = data.matrix(myData[,-1])  
Y = myData[,1] 

# all.test.id: ntestxT matrix, each column records 
ntest = round(n * 0.25)  # test set size
ntrain = n-ntest  # training set size
all.test.id = matrix(0, ntest, T)  # 
for(t in 1:T){
  all.test.id[, t] = sample(1:n, ntest)
}
save(all.test.id, file="alltestID.RData")

test.id = all.test.id[,1] 

## Method 1: Full Model (Full)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id,])
Ytest.pred = predict(full.model, newdata = myData[test.id,])
mean((Y[test.id] - Ytest.pred)^2)
proc.time() - start.time

## Method 2: Forward AIC (AIC.F)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id, ])
stepAIC = step(lm(Y ~ 1, data = myData[-test.id, ]), 
               list(upper = full.model),
               trace = 0, direction = "forward")
Ytest.pred = predict(stepAIC, newdata = myData[test.id, ])

# number of predictors (excluding the intercept)
length(stepAIC$coef) - 1  
proc.time() - start.time


## Method 3: Backward AIC (AIC.B)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id, ])
stepAIC = step(full.model, trace = 0, direction = "backward")
Ytest.pred = predict(stepAIC, newdata = myData[test.id, ])
length(stepAIC$coef) - 1
proc.time() - start.time

## Method 4: Forward BIC (BIC.F)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id, ])
stepAIC = step(lm(Y ~ 1, data = myData[-test.id, ]),
               list(upper = full.model),
               trace = 0, direction = "forward", k = log(ntrain))
Ytest.pred = predict(stepAIC, newdata = myData[test.id, ])

# number of predictors (excluding the intercept)
length(stepAIC$coef) - 1 
proc.time() - start.time

## Method 5: Backward BIC (BIC.B)
start.time = proc.time()
full.model = lm(Y ~ ., data = myData[-test.id, ])
stepAIC = step(full.model, trace = 0,
               direction = "backward", k = log(ntrain))
Ytest.pred = predict(stepAIC, newdata = myData[test.id, ])
length(stepAIC$coef) - 1
proc.time() - start.time

## Method 6/7: Ridge with lambda_min/lambda_1se (R_min, R_1se)
start.time = proc.time()
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
best.lam = cv.out$lambda.min
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
proc.time() - start.time
# Exclude the computation for DF when recording computing time for Ridge,

# DF for Ridge
# DF = tr(S) where S = X(X^t X + lam*I)^{-1}X^t and X is the standardized design 
# matrix with each column having mean zero and sd 1. Note that glmnet computes
# sd using denominator n not (n-1). 
# In addition, the objective function in glmnet for ridge is slightly different
# from the one used in class. So lam used in S (above) corresponds to lambda.min
# or lambda.1se times the sample size

ntrain = n - dim(all.test.id)[1]
tmpX = scale(X[-test.id, ]) * sqrt(ntrain / (ntrain - 1))
d = svd(tmpX)$d 

# df for Ridge with lambda_min
best.lam = cv.out$lambda.min
sum(d^2/(d^2 + best.lam*ntrain))

# df for Ridge with lambda_1se
best.lam = cv.out$lambda.1se
sum(d^2/(d^2 + best.lam*ntrain))

## Method 8: Lasso using lambda.min (L_min)
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
best.lam = cv.out$lambda.min
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
mean((Ytest.pred - Y[test.id])^2)
mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
sum(mylasso.coef != 0) - 1  # size of Lasso with lambda.min


## Method 9/10: Lasso using lambda.1se without/with Refit (L_1se, L_Refit)
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
best.lam = cv.out$lambda.1se
Ytest.pred = predict(cv.out, s = best.lam, newx = X[test.id, ])
mean((Ytest.pred - Y[test.id])^2)
mylasso.coef = predict(cv.out, s = best.lam, type = "coefficients")
sum(mylasso.coef != 0) - 1 # size of Lasso with lambda.1se

var.sel = row.names(mylasso.coef)[nonzeroCoef(mylasso.coef)[-1]]
tmp.X = X[, colnames(X) %in% var.sel]
mylasso.refit = coef(lm(Y[-test.id] ~ tmp.X[-test.id, ]))
Ytest.pred = mylasso.refit[1] + tmp.X[test.id,] %*% mylasso.refit[-1]
mean((Ytest.pred - Y[test.id])^2)

