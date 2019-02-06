# Setup
mypackages = c("MASS", "glmnet")   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)
set.seed(2134)

# Data preparation
myData = Boston
names(myData)[14] = "Y"
iLog = c(1, 3, 5, 6, 8, 9, 10, 14);
myData[, iLog] = log(myData[, iLog]);
myData[, 2] = myData[, 2] / 10;
myData[, 7] = myData[, 7]^2.5 / 10^4
myData[, 11] = exp(0.4 * myData[, 11]) / 1000;
myData[, 12] = myData[, 12] / 100;
myData[, 13] = sqrt(myData[, 13]);

# Move the last column of myData, the response Y, to the 1st column.
#assign last column to Y and combine it with the rest
myData = data.frame(Y = myData[,14], myData[,-14]); names(myData)[1] = "Y";
names(myData)
n = dim(myData)[1]; 
p = dim(myData)[2]-1;
X = as.matrix(myData[, -1]);  # some algorithms need the matrix/vector 
Y = myData[, 1];              # input (instead of data frame)

# Split the data into two parts: 80% for training and 20% for testing
ntest = round(n*0.2)
ntrain = n - ntest;
test.id = sample(1:n, ntest);
Ytest = myData[test.id, 1]

# Full Model
full.model = lm( Y ~ ., data = myData[-test.id, ]);  
Ytest.pred = predict(full.model, newdata= myData[test.id, ]);
sum((Ytest - Ytest.pred)^2)/ntest # averaged MSE on the test set

# Lasso using glmnet; lambda chosen by 10-fold CV.
mylasso = glmnet(X[-test.id,], Y[-test.id], alpha = 1)
summary(mylasso)

# plotting
par(mfrow = c(1, 2))
plot(mylasso, label=TRUE, xvar = "norm")
plot(mylasso, label=TRUE, xvar = "lambda")

par(mfrow=c(1,1))
mylasso$df

cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
plot(cv.out)
# Try our own lambda sequences.
# lam.seq =  exp(seq(-4, 2, length=100))
# lam.seq =  exp(seq(-6, -1, length=100))
# cv.out = cv.glmnet(X[-test.id,], Y[-test.id], alpha = 1, lambda = l am.seq)
# plot(cv.out)

# Check how lambda.min and lambda.1se are computed.
cv.out$lambda.min
tmp.id=which.min(cv.out$cvm)
cv.out$lambda[tmp.id]

cv.out$lambda.1se
max(cv.out$lambda[cv.out$cvm < cv.out$cvm[tmp.id] + cv.out$cvsd[tmp.id]])

# Retrieve Lasso coefficients.
mylasso.coef.min = predict(mylasso, s=cv.out$lambda.min, type="coefficients")
mylasso.coef.1se = predict(mylasso, s=cv.out$lambda.1se, type="coefficients")
cbind(mylasso.coef.min, mylasso.coef.1se)

# number of variables selected (including the intercept)
sum(mylasso.coef.1se != 0)
# names of selected non-intercept variables
row.names(mylasso.coef.1se)[nonzeroCoef(mylasso.coef.1se)[-1]]

# Apply the fitted model for prediction on the test data.
mylasso = glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
Ytest.pred = predict(mylasso, s = cv.out$lambda.min, newx = X[test.id,])
mean((Ytest.pred - Y[test.id])^2)

Ytest.pred = predict(mylasso, s = cv.out$lambda.1se, newx = X[test.id,])
mean((Ytest.pred - Y[test.id])^2)

# Refit the model selected by Lasso to reduce bias.
# Variables selected by lambda.1se 
mylasso.coef.1se = predict(mylasso, s = cv.out$lambda.1se, type="coefficients")
var.sel = row.names(mylasso.coef.1se)[nonzeroCoef(mylasso.coef.1se)[-1]]
var.sel; 

tmp.X = X[, colnames(X) %in% var.sel]
mylasso.refit = coef(lm(Y[-test.id] ~ tmp.X[-test.id, ]))
Ytest.pred = mylasso.refit[1] + tmp.X[test.id,] %*% mylasso.refit[-1]
mean((Ytest.pred - Y[test.id])^2)
