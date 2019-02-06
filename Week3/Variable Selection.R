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

# Fit the full model
full.model = lm( Y ~ ., data = myData);  
n = dim(myData)[1];
summary(full.model)
# predictor number
p = 13

# Level-wise search
library(leaps)
b = regsubsets(Y ~ ., data=myData, nvmax = p)
rs = summary(b)
rs$which
# plotting
msize = 1:p;
par(mfrow=c(1,2))
Aic = n*log(rs$rss/n) + 2*msize;
Bic = n*log(rs$rss/n) + msize*log(n);
plot(msize, Aic, xlab="No. of Parameters", ylab = "AIC");
plot(msize, Bic, xlab="No. of Parameters", ylab = "BIC");
# check variable 
cbind(rs$which[which.min(Aic),], rs$which[which.min(Bic), ])
#check different BIC formulae
cbind(rs$bic, Bic, rs$bic - Bic)
# which are the 2nd and 3th best model using AIC/BIC
b = regsubsets(Y ~ ., data=myData, nbest = 3, nvmax = p)
rs = summary(b)
rs$which
# plotting
msize = apply(rs$which, 1, sum) - 1
par(mfrow=c(1,2))
Aic = n*log(rs$rss/n) + 2*msize;
Bic = n*log(rs$rss/n) + msize*log(n);
plot(msize, Aic, xlab="No. of Parameters", ylab = "AIC");
plot(msize, Bic, xlab="No. of Parameters", ylab = "BIC");
plot(msize[msize > 3], Aic[msize > 3], xlab="No. of Parameters", ylab = "AIC");
plot(msize[msize > 3], Bic[msize > 3], xlab="No. of Parameters", ylab = "BIC");
# top three models by AIC
rs$which[order(Aic)[1:3],]
# top three models by BIC
rs$which[order(Bic)[1:3],]

# Stepwise AIC
stepAIC = step(full.model, direction="both")

# Stepwise BIC
n = dim(myData)[1]
stepBIC = step(full.model, direction="both", k=log(n))     

sel.var.AIC = attr(stepAIC$terms, "term.labels")
sel.var.BIC = attr(stepBIC$terms, "term.labels")
sel.var.AIC
sel.var.BIC
length(sel.var.AIC)
length(sel.var.BIC)
c("rad", "hello") %in% sel.var.AIC
sel.var.BIC %in% sel.var.AIC
