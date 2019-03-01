setwd("~/Documents/2_UIUC/8_2019_Spring/STAT 542 Statistical Learning/Week 6")
source("sportsranks.txt");
head(sportsranks)

plot(hclust(dist(sportsranks)), xlab="Complete Linkage", sub="");
plot(hclust(dist(sportsranks), method="single"), xlab="Single Linkage", sub="");
plot(hclust(dist(sportsranks), method="average"), xlab="Average Linkage", sub="");

clusters = hclust(dist(sportsranks), method="average")
clusterCut = cutree(clusters, 3)
table(clusterCut)

# Check how hclust results differ from the ones from K-means.
km2=kmeans(sportsranks, centers=2, nstart=10);
km3=kmeans(sportsranks, centers=3, nstart=10);

table(cutree(clusters, 2), km2$cluster)
table(cutree(clusters, 3), km3$cluster)

clusters = hclust(dist(sportsranks))
table(cutree(clusters, 2), km2$cluster)
table(cutree(clusters, 3), km3$cluster)
