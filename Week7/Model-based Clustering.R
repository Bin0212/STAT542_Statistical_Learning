setwd("~/Documents/2_UIUC/8_2019_Spring/STAT 542 Statistical Learning/Week 7")

data=read.table("faithful.dat", header=TRUE)
plot(data[,1], data[,2], xlab="Interval", ylab="Duration")

library(mclust)
# citation("mclust")
?em
?mclustModelNames

# Try K=2 with model “VVI”
# ??diagonal, varying volume and shape
# ??what about higher p
# ?? no iterate
K=2; n=length(data[,1])
z=matrix(0,n, K); 
id=sample(1:K, n, replace=TRUE);
for(i in 1:n) z[i,id[i]]=1;

tmp=mstep(modelName="VVI", data, z)
myout=em(modelName="VVI", data, parameters=tmp$parameters);

surfacePlot(data, parameters=myout$parameters, nlevels=10)
points(data, col="blue")

# Try K=3 with model “VVI”
# ?? why only two model
K=3; n=length(data[,1])
z=matrix(0,n, K); 
id=sample(1:K, n, replace=TRUE);
for(i in 1:n) z[i,id[i]]=1;

tmp=mstep(modelName="VVI", data, z)
myout=em(modelName="VVI", data, parameters=tmp$parameters);
surfacePlot(data, parameters=myout$parameters)
points(data, col="blue")

# Use BIC to select both K and model.
geyserBIC=mclustBIC(data, G=1:5)
plot(geyserBIC)
summary.mclustBIC(geyserBIC, data)
# Plot the best model selected by BIC
#? Why BIC larger is better
BICbest=mclustModel(data, geyserBIC)
coordProj(data,parameters=BICbest$parameters, what=c("classification"), z=BICbest$z)

# The top two models selected by BIC are “EEE”, however, when we use the EEE 
# model to cluster a subset of the data, we find the clustering result is not 
# stable.

mysubset=sample(1:n, n/2);
newdata=data[mysubset,]
newbest=Mclust(newdata, modelNames="EEE")
par(mfrow=c(1,2))
plot(newbest, newdata, what="BIC")
coordProj(newdata,parameters=newbest$parameters, what=c("classification"), z=newbest$z)

# The VVV model more stable than the EEE model. So I would suggest to choose a 
# two-component VVV model.

mysubset=sample(1:n, n/2);
newdata=data[mysubset,]
newbest=Mclust(newdata, modelNames="VVV")
par(mfrow=c(1,2))
plot(newbest, newdata, what="BIC")
coordProj(newdata,parameters=newbest$parameters, what=c("classification"), z=newbest$z)
