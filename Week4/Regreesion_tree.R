library(ggplot2)
library(rpart)
library(rpart.plot)
library(tree) 
library(MASS)
library(glmnet)

myData = Boston
names(myData)[14] = "Y"
iLog = c(1, 3, 5, 6, 8, 9, 10, 14);
myData[, iLog] = log(myData[, iLog]);
myData[, 2] = myData[, 2] / 10;
myData[, 7] = myData[, 7]^2.5 / 10^4
myData[, 11] = exp(0.4 * myData[, 11]) / 1000;
myData[, 12] = myData[, 12] / 100;
myData[, 13] = sqrt(myData[, 13]);
X = as.matrix(myData[, -14])
y = myData$Y
Housing1 = myData

set.seed(1234)
tr1 = rpart(Y ~ ., data = Housing1)
par(mfrow=c(1,2))
plot(tr1)
rpart.plot(tr1)

printcp(tr1)

prune(tr1, cp=0.3)
prune(tr1, cp=0.2)
prune(tr1, cp=0.156)
par(mfrow=c(1,1))
plotcp(tr1)

tr2 = rpart(Y ~ ., data = Housing1, 
            control = list(cp = 0, xval = 10))
plot(tr2)
printcp(tr2)
plotcp(tr2)
# get index of CP with lowest xerror
opt = which.min(tr2$cptable[, "xerror"])  # 28
# get the optimal CP value
tr2$cptable[opt, 4]

# upper bound for equivalent optimal xerror
tr2$cptable[opt, 4] + tr2$cptable[opt, 5]

# row IDs for CPs whose xerror is equivalent to min(xerror)
tmp.id = which(tr2$cptable[, 4] <= tr2$cptable[opt, 4] +
                 tr2$cptable[opt, 5])
# CP.1se = any value between row (tmp.id) and (tmp.id-1)
CP.1se = 0.0032

# Prune tree with CP.1se
tr3 = prune(tr2, cp = CP.1se)

cbind(tr2$cptable[, 1], c(-diff(tr2$cptable[, 3]), 0))

?predict.rpart 

# Handle categorical predictors
set.seed(1234)
n = nrow(Housing1); 
m = 30 
X = as.factor(sample(1:m, n, replace = TRUE))
tmp = data.frame(Y = Housing1$Y, X = X)
myfit = rpart(Y ~ X, data = tmp)
myfit

group.mean = as.vector(tapply(tmp$Y, tmp$X, mean))
order(group.mean)
group.mean[order(group.mean)] #same as sort(group.mean)

tmp$Z= rnorm(n)  # add a numerical feature
myfit = rpart(Y ~ X + Z, data = tmp)
rpart.plot(myfit)
myfit
id1 = which(! (tmp$X %in% c(1, 22, 25, 26, 6, 21)))
length(id1)  # 419
id2 = id1[which(tmp$Z[id1] > 0.95)]
length(id2)  # 54
group.mean = as.vector(tapply(tmp$Y[id2], tmp$X[id2], mean))
order(group.mean)
group.mean
