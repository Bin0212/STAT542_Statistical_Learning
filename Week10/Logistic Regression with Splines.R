####################################################### Prepare the Phoneme Data
#install.packages("ElemStatLearn")
library(ElemStatLearn)
library(splines)
data(phoneme)

mydata = phoneme[phoneme$g %in% c("aa", "ao"), ]
# mydata[1, ]
mydata[1, c(1:5, 250:258)]

X = data.matrix(mydata[, -c(257, 258)])
Y = mydata[, 257]
table(Y)

Y = as.numeric(Y)-1
table(Y)

# Fit a Logitsic Regressio Model
logfit1 = glm(Y ~ X, family="binomial")
coef1 = logfit1$coef[-1]

# Fit a Logistic Regression Model with Coefficients Modeled by Splines
H = ns(1:256, df = 12)
B = X %*% H
logfit2 = glm(Y ~ B, family="binomial")
coef2 = H %*% logfit2$coef[-1]

plot(1:256, coef1, type="n", 
     xlab="Frequency", ylab="Logistic Regression Coefficients")
lines(1:256, coef1, col="gray")
lines(1:256, coef2, col="red")

# Compare Classification Performance
T = 5
n = dim(X)[1]
Err1 = matrix(0, T, 2)  # col 1: training; col 2: test
Err2 = matrix(0, T, 2)  # col 1: training; col 2: test
set.seed(123)
for(t in 1:T){
  testid = sample(1:n, round(n*0.2))
  
  logfit1 = glm(Y[-testid] ~ X[-testid, ], family="binomial")
  yhat1 = as.numeric(logfit1$fitted.values>0.5)
  Err1[t, 1] = mean(yhat1 != Y[-testid])
  yhat1 = X[testid, ] %*% logfit1$coef[-1] + logfit1$coef[1]
  yhat1 = as.numeric(yhat1>0.5)
  Err1[t, 2] = mean(yhat1 != Y[testid])
  
  logfit2 = glm(Y[-testid] ~ B[-testid, ], family="binomial")
  yhat2 = as.numeric(logfit2$fitted.values>0.5)
  Err2[t, 1] = mean(yhat2 != Y[-testid])
  yhat2 = B[testid, ] %*% logfit2$coef[-1] + logfit2$coef[1]
  yhat2 = as.numeric(yhat2>0.5)
  Err2[t, 2] = mean(yhat2 != Y[testid])
}

# method 1 overfitted
round(cbind(Err1, Err2), dig=3)
