#X2 is independent of Y, but X2 can be a significant predictor for Y in 
#a multiple linear regression model when it can help to remove the noise in 
#other predictors that is irrelevant to Y. 
set.seed(2500)
n = 50
Z1 = rnorm(n); Z2 = rnorm(n)
Y = Z1 + rnorm(n)
X1 = Z1 + Z2
X2 = Z2 
summary(lm(Y ~ X2))
summary(lm(Y ~ X1 + X2))

#X1 and X2 are highly correlated, and the correlated part is relevant to Y. 
#So individually, they are significant, but are not when both are included in 
#the model.
set.seed(500)
n = 50
Z1 = rnorm(n);
Y = Z1 + rnorm(n)
X1 = Z1 + 0.25*rnorm(n)
X2 = Z1 + 0.25*rnorm(n)
summary(lm(Y ~ X1))
summary(lm(Y ~ X2))
summary(lm(Y ~ X1 + X2))
