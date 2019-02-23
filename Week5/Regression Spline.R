library(splines);
library(ggplot2)
help(bs)
help(ns)

setwd("~/Documents/2_UIUC/8_2019_Spring/STAT 542 Statistical Learning/Week 5")

# Spline Basis Functions
  # Basis functions for cubic splines with 5 knots and df = 9.
x = (1:199)/100;
n = length(x)
m = 5;
myknots = 2*(1:m)/(m+1)
myknots

X = cbind(1, x, x^2, x^3);
for(i in 1:m){
  tmp = (x-myknots[i])^3;
  tmp[tmp<0] = 0;
  X = cbind(X, tmp);
}
plot(c(0,2), range(X), type="n", xlab="", ylab="")
title("Truncated Power Basis")

for(i in 1:(m+4)){
  tmp = X[,i];
  if (i<=4) mylty=1 else mylty=2;
  lines(x[tmp!=0], tmp[tmp!=0], col=i, lty=mylty, lwd=2)
}
for(i in 1:m){
  points(myknots[i], 0, pty="m", pch=19, cex=2)
}

F = bs(x,knots = myknots, intercept = TRUE)
dim(F)
mydf = m+4; 
tmpdata = data.frame(t = rep(1:n, mydf),
                     basisfunc=as.vector(F), 
                     type=as.factor(rep(1:mydf, each=n)))
ggplot(tmpdata, aes(x=t, y=basisfunc, color=type)) + geom_path()

  # If we do not set intercept = TRUE, 
  # then bs will return 9-1 = 8 columns.
F = bs(x, knots = myknots)
dim(F)
mydf = m+3; 
tmpdata = data.frame(t = rep(1:n, mydf),
                     basisfunc=as.vector(F), 
                     type=as.factor(rep(1:mydf, each=n)))
ggplot(tmpdata, aes(x=t, y=basisfunc, color=type)) + geom_path()

  # Basis functions for NCS with 7 knots (5 interior knots and 2 boundary knots) 
  # and df = 7
F = ns(x, knots=myknots, Boundary.knots=c(0,2), intercept=TRUE)
dim(F)
mydf = 7
tmpdata = data.frame(t = rep(1:n, mydf),
                     basisfunc=as.vector(F), 
                     type=as.factor(rep(1:mydf, each=n)))
ggplot(tmpdata, aes(x=t, y=basisfunc, color=type)) + geom_path()

# The Birthrates Data
  # This dataset lists the number of live births per 10,000 23-year-old women in 
  # the United States between 1917 and 2003.
source("birthrates.txt");
birthrates = as.data.frame(birthrates)
names(birthrates) = c("year", "rate")
ggplot(birthrates, aes(x=year, y=rate)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)

  # understand how R counts the df
  # knots = 2
fit1 = lm(rate~bs(year, knots=quantile(year, c(1/3, 2/3))), data=birthrates);
  # no intercept, df = knots + 3
fit2 = lm(rate~bs(year, df=5), data=birthrates);
  # with intercept, df = knots + 4
fit3 = lm(rate~bs(year, df=6, intercept=TRUE), data=birthrates) 
fit4 = lm(rate~bs(year, df=5, intercept=TRUE), data=birthrates)

plot(birthrates$year, birthrates$rate, ylim=c(90,280))
lines(spline(birthrates$year, predict(fit1)), col="red", lty=1)
lines(spline(birthrates$year, predict(fit2)), col="blue", lty=2)
lines(spline(birthrates$year, predict(fit3)), col="green", lty=3)
lines(spline(birthrates$year, predict(fit4)), lty=2, lwd=2)

  # Alternatively, you can predict the spline fit on a fine grid, and then connect them
plot(birthrates$year, birthrates$rate, ylim=c(90,280))
year.grid = seq(from=min(birthrates$year), to=max(birthrates$year), length=200)
ypred = predict(fit1, data.frame(year=year.grid))
lines(year.grid, ypred, col="blue", lwd=2)

  # knots = 4
fit1=lm(rate~ns(year, knots=quantile(year, (1:4)/5)), data=birthrates);
  # df = knots + 1 without intercept
fit2=lm(rate~ns(year, df=5), data=birthrates);
  # df = knots + 2 with intercept
fit3=lm(rate~ns(year, df=6, intercept=TRUE), data=birthrates) 

plot(birthrates$year, birthrates$rate, ylim=c(90,280))
lines(spline(birthrates$year, predict(fit1)), col="red", lty=1)
lines(spline(birthrates$year, predict(fit2)), col="blue", lty=2)
lines(spline(birthrates$year, predict(fit3)), col="green", lty=3)

  # Try cubic splines with different degree-of-freedoms
plot(birthrates$year, birthrates$rate, ylim=c(90,280));
lines(spline(birthrates$year, predict(lm(rate~bs(year, df=7), data=birthrates))), col="blue");
lines(spline(birthrates$year, predict(lm(rate~bs(year, df=14), data=birthrates))), col="red");
lines(spline(birthrates$year, predict(lm(rate~bs(year, df=19), data=birthrates))), col="black");
legend("topright", lty=rep(1,3), col=c("blue", "red", "black"), legend=c("df=8", "df=15", "df=20"))

  # Make prediction outside the data range
new = data.frame(year=1905:2015);
fit1=lm(rate~bs(year, df=7), data=birthrates);
pred1=predict(fit1, new);

fit2=lm(rate~ns(year, df=7), data=birthrates);
pred2=predict(fit2, new);
plot(birthrates$year,birthrates$rate, xlim=c(1905,2015),
     ylim=c(min(pred1,pred2), max(pred1,pred2)), 
     ylab="Birth Rate", xlab="Year") 
lines(new$year, pred1, col="red")
lines(new$year, pred2, col="blue")
legend("bottomleft", lty=rep(1,2),  col=c("red",  "blue" ), legend=c("CS with df=7", "NCS with df=7"))

  # Use 10-fold CV to select df (or equivalently the number of knots)
  # The location of knots will affect the performance of a spline model. But 
  # selecting the location of knots is computationally too expensive. Instead, 
  # we place knots equally at quantiles of x, and then select just the number 
  # of knots, or equivalently, the df. Can we use F-test to select the number 
  # of knots? 
  # No, we can't use F-test to selection df, because they are not nested.
  
  # For each df, we use 10-fold CV to calculate the CV error. When doing 10-fold 
  # CV, each time, based on 90% of the data, we place the (df-4) knots at the 
  # corresponding quantiles and then fit a regression spline,

  # First, we need to divide the data into K folds.
K=10
n = nrow(birthrates)
fold.size = c(rep(9, 7), rep(8, 3))
fold.id = rep(1:K, fold.size)
fold.id

fold.id = fold.id[sample(1:n, n)]
fold.id

mydf = 10:30
mycv = rep(0, length(mydf))

for(i in 1:length(mydf)){
  m = mydf[i]-4;  
  for(k in 1:K){
    id = which(fold.id == k);
    myknots = quantile(birthrates$year[-id], (1:m)/(m+1))
    myfit = lm(rate ~ bs(year, knots=myknots),
               data=birthrates[-id,])
    ypred = predict(myfit, newdata=birthrates[id,])
    mycv[i]=mycv[i] + sum((birthrates$rate[id] - ypred)^2)
  }
}
plot(mydf, mycv)
  # Re-run the 10-fold CV. The plot of mydf versus mycv may vary, but shouldn’t 
  # be too different.

