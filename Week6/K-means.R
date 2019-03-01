setwd("~/Documents/2_UIUC/8_2019_Spring/STAT 542 Statistical Learning/Week 6")
source("sportsranks.txt");
head(sportsranks)
########### K-means ########### 
# Run K-means with K=2 and 3. hstart: how many random sets should be chosen
km2=kmeans(sportsranks, centers=2, nstart=10);
km3=kmeans(sportsranks, centers=3, nstart=10);

D=dist(sportsranks)
Xmds=cmdscale(D);

par(mfrow=c(1,2));
plot(Xmds[,1], Xmds[,2], type="n", xlab="", ylab="")
points(Xmds[,1], Xmds[,2], pch=km2$cluster, col=km2$cluster);
title("MDS plot for K=2")
plot(Xmds[,1], Xmds[,2], type="n", xlab="", ylab="");
points(Xmds[,1], Xmds[,2], pch=km3$cluster, col=km3$cluster);
title("MDS plot for K=3")

# How is cMDS computed?
D2 = as.matrix(D^2)
n = dim(D2)[1]
tmp = D2 - matrix(colMeans(D2), nrow=n, ncol=n, byrow=TRUE) - 
  matrix(rowMeans(D2), nrow=n, ncol=n, byrow=FALSE)
tmp = -(tmp + mean(D2))/2
tmp.svd = svd(tmp)
F = tmp.svd$u[, 1:2]  %*% diag(sqrt(tmp.svd$d[1:2]))
cor(F, Xmds)

########### Gap Statistics ########### 
library(cluster)
set.seed(123)
# B: integer, number of Monte Carlo ("bootstrap") samples.
gap_stat = clusGap(sportsranks, FUN = kmeans, nstart = 25,
                   K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
par(mfrow=c(1,1));
plot(gap_stat, frame = FALSE, xlab = "Number of clusters k")
abline(v = 2, lty = 2)

# Next we compute gap statistic step by step.
# Step 1. Calculate Within-Cluster-SS for different K
# SS: sum of squared distance
n=130;
SS=rep(0,10)
SS[1]=sum(diag(var(sportsranks)))*(n-1);
for(k in 2:10){
  kms=kmeans(sportsranks, centers=k, nstart=10);
  SS[k]=sum(kms$withinss);
}
lss=log(SS);

# Step 2. Calculate SS_0(K) under the null hypothesis
n = dim(sportsranks)[1] 
m = 7  # each review is a permutation of numbers from 1 to 7
B = 50 # number of iterations
lss0 = matrix(0,B,10)
for(b in 1:B){
  xstar=NULL
  for(i in 1:n){ xstar = rbind(xstar, sample(m)) }
  lss0[b,1] = log(sum(diag(var(xstar)))*(n-1))
  for(k in 2:10){
    kms = kmeans(xstar, centers=k, nstart=10)
    lss0[b,k] = log(sum(kms$withinss))
  }
}

plot(1:10, lss, type="l", col="red", xlab="k", ylab="log(Within SS)");
for(b in 1:B){ points(1:10, lss0[b,], type="l", col=8) }

# Step 3. Gap curve. Based on the 1se rule, the optimal K = 2.
lss0m = apply(lss0, 2, mean);
lss0sd = sqrt(apply(lss0, 2, var))*sqrt(1+1/B);
diff = lss0m-lss;
matplot(1:10, cbind(diff, diff+lss0sd, diff-lss0sd), type="l",
        xlab="k", ylab="Gap")
points(1:10, diff);
points(1:10, diff-lss0sd, pch="+", col="green");

########### Silhouette Plots ########### 
# s(i) close to 1: object i is well classified;
sil2=silhouette(km2$cluster,D);
sil3=silhouette(km3$cluster,D);
par(mfrow=c(1,2))
plot(sil2)
plot(sil3)
par(mfrow=c(1,1))

# Compute and plot average silhouette over a range of K values.
k.max = 10
sil = rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  tmp = kmeans(sportsranks, centers = i, nstart = 10)
  ss <- silhouette(tmp$cluster, dist(sportsranks))
  sil[i] <- mean(ss[, 3])
}

# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)

# Prediction Strength
# cutoff: numeric between 0 and 1. The optimal number of clusters is the maximum 
# one with prediction strength above cutoff.
library(fpc)
ps = prediction.strength(sportsranks, Gmax=10,
                         clustermethod=kmeansCBI)
## Warning: did not converge in 100 iterations
plot(1:10, ps$mean.pred, type='b', ylim=c(0,1), 
     xlab='Number of clusters', ylab='Prediction strength')
abline(h=ps$cutoff, col="red", lty=2, lwd=2)
