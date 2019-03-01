setwd("~/Documents/2_UIUC/8_2019_Spring/STAT 542 Statistical Learning/Week 6")
source("sportsranks.txt");
head(sportsranks)
D=dist(sportsranks)
Xmds=cmdscale(D);

pam2=pam(sportsranks, k=2);
plot(pam2)

pam3=pam(sportsranks, k=3);
plot(pam3)

par(mfrow=c(1,2));
plot(Xmds[,1], Xmds[,2], type="n");
points(Xmds[,1], Xmds[,2], pch=pam2$clustering, col=pam2$clustering);
title("MDS plot for K=2")
plot(Xmds[,1], Xmds[,2], type="n");
points(Xmds[,1], Xmds[,2], pch=pam3$clustering, col=pam3$clustering);
title("MDS plot for K=3")

# compare with Kmeans
km2=kmeans(sportsranks, centers=2, nstart=10);
km3=kmeans(sportsranks, centers=3, nstart=10);

table(pam2$clustering, km2$cluster)
table(pam3$clustering, km3$cluster)
