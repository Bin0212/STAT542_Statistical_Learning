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

# Ridge using glmnet; lambda chosen by 10-fold CV (already use standardization)
myridge = glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
plot(myridge, label = TRUE, xvar = "lambda")
summary(myridge)

length(myridge$lambda)  # retrieve the lambda value
dim(myridge$beta)       # coefficients for 13 non-intercept predictors
length(myridge$a0)      # intercept

# the 13 coefficients (including intercept) can also be retrieved using
# coef(myridge)
dim(coef(myridge))

# The two coefficient matrices should be the same
sum((coef(myridge) - rbind(myridge$a0, myridge$beta))^2)

# Ridge regression coefs could change sign along the path
round(myridge$beta[8, ], dig = 2)

# How are the intercepts computed?
k = 2; 
my.mean = apply(X[-test.id, ], 2, mean)  # 13x1 mean vector for training X
mean(Y[-test.id]) - sum(my.mean * myridge$beta[, k])

myridge$a0[k]  # intercept for lambda = myridge$lambda[k]

# Check whether our intercept formula is true for all intercepts 
sum((mean(Y[-test.id]) - my.mean %*% myridge$beta  - myridge$a0)^2)

# Selection lambda by 10-fold CV. The CV results are stored in
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0) 
plot(cv.out)
# change lambda range since the above isn't a "U" shape
lam.seq = exp(seq(-6, 2, length=100))
cv.out = cv.glmnet(X[-test.id,], Y[-test.id], alpha=0, lambda=lam.seq)  
plot(cv.out)

names(cv.out)
cv.out$lambda[which.min(cv.out$cvm)]
cv.out$lambda.min
tmp.id = which.min(cv.out$cvm)
max(cv.out$lambda[cv.out$cvm < cv.out$cvm[tmp.id] + cv.out$cvsd[tmp.id]])
cv.out$lambda.1se

# Evaluate prediction performance
myridge = glmnet(X[-test.id,], Y[-test.id], alpha=0, lambda = lam.seq)
Ytest.pred = predict(myridge, s = cv.out$lambda.1se, newx=X[test.id,])
mean((Ytest.pred - Y[test.id])^2)
Ytest.pred=predict(myridge, s = cv.out$lambda.min, newx=X[test.id,])
mean((Ytest.pred - Y[test.id])^2)
