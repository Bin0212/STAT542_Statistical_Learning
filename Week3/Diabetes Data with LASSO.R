library("care")
library(glmnet)
set.seed(1234)
X = data.matrix(efron2004$x)
Y = efron2004$y
mylasso = glmnet(X, Y, alpha = 1)
mylasso$df 

lasso.coef = as.matrix(coef(mylasso))
# round(lasso.coef[, -c(1:60)], dig=2)
image(coef(mylasso))

lasso.coef = as.matrix(coef(mylasso))
lasso.varset = ifelse(lasso.coef==0, 0, 1)
lasso.varset

library(leaps)
RSSleaps = regsubsets(X, Y, nbest=1, nvmax=10)
sumleaps = summary(RSSleaps, matrix=T)
sumleaps

n = length(Y)
msize=apply(sumleaps$which, 1, sum);
BIC = sumleaps$bic; 
# which is happening here?
BIC = BIC - min(BIC); BIC = BIC/max(BIC);
AIC = n*log(sumleaps$rss/n) + 2*msize;
# which is happening here?
AIC = AIC - min(AIC); AIC = AIC/max(AIC);
cbind(AIC, BIC)

plot(range(msize), c(0,1.1), type="n", xlab="Model Size (with Intercept)", ylab="Model Selection Criteria")
points(msize, AIC, col="blue")
points(msize, BIC, col="black")
legend("topright", lty=rep(1,3), col=c("blue", "black"), legend=c("AIC", "BIC"))
