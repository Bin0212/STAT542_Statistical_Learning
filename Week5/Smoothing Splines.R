library(splines)
help(smooth.spline)
options(digits = 4)

# A simulated Example (ESL, chap 5.5.2)
  # (x_i,y_i): data points (i=1:30)
  # (fx_j, fy_j): true function evaluated on a fine grid (j=1:50)
set.seed(1234)
n = 30 
err = 1
x = sort(runif(n))
y = sin(12*(x+0.2))/(x+0.2) + rnorm(n, 0, err);
plot(x, y, col="red");

fx = 1:50/50;
fy = sin(12*(fx+0.2))/(fx+0.2)
lines(fx, fy, col=8, lwd=2);

# Fit a smoothing spline model
  # Fit smoothing spline models with various dfs.
par(mfrow=c(2,2));      # 2x2 (totally 4) subplots
plot(x,y, xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, df=5),fx),  lty=2, col='blue', lwd=1.5);
title('df=5');

plot(x,y,xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, df=9),fx),  lty=2, col='blue', lwd=1.5);
title('df=9');

plot(x,y,xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, df=15),fx),  lty=2, col='blue', lwd=1.5);
title('df=15');

plot(x,y,xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, df=20),fx),  lty=2, col='blue', lwd=1.5);
title('df=20')

# Demmler & Reinsch Basis
  # Here is how we obtain the DR basis: we first obtain the smoother matrix S 
  # (which is not returned y R, so we write our own script to compute it), and 
  # then the eigen-vectors of S are basically the DR basis functions.
smooth.matrix = function(x, df){
  # return the smoother matrix with knots x and degree of freedom = df
  # this function is for x having unique values
  n = length(x);
  A = matrix(0, n, n);
  for(i in 1:n){
    y = rep(0, n); y[i]=1;
    yi = smooth.spline(x, y, df=df)$y;
    A[,i]= yi;
  }
  return((A+t(A))/2)
}
fx = 1:50/50
S4 = smooth.matrix(fx, df=4);
S9 = smooth.matrix(fx, df=9);
tmp = ns(fx, df=9, intercept=TRUE)
H9 = tmp%*%solve(t(tmp)%*%tmp)%*%t(tmp);

  # Obtain the eigen value and eigen vector of the smoother/projection matrices. 
  # The eigen vectors of S4 and S9 should be the same, up to a sign flip.

eigen.S4 = eigen(S4);
eigen.S9 = eigen(S9);
eigen.H9 = eigen(H9);

v4 = eigen.S4$ve;
v9 = eigen.S9$ve;

par(mfrow=c(3,5));
for(i in 1:15) {
  plot(c(min(x), max(x)),c(min(v4, v9), max(v4, v9)), xlab="x", ylab="", 
       type="n");
  lines(fx, v4[,i], col=2,lty=1, lwd=2.5);
  lines(fx, v9[,i], col=3, lty=2, lwd=2.5);}

par(mfrow=c(1,1));
  # Plot the eigen values: Note the first two eigen values are always 1.
plot(eigen.H9$va, pch=5, col="black", cex=1);
points(eigen.S4$va, , col="red", xlab='', ylab='eigen values',
       cex=1.5);
points(eigen.S9$va, pch=4, col="blue", cex=1);
lines(c(0,n), c(1, 1), col=8, lty=2, lwd=1);
legend("topright", pch=c(1,4,5), col=c("red", "blue", "black"),
       legend=c("SS with df=4", "SS with df=9", "NCS with df=9"))

  # Check for the effective degree of freedom
sum(diag(S4))
sum(diag(S9))
sum(diag(H9))

# LOO-CV and GCV
fit = smooth.spline(x, y, df=9);
fit$df
sum(fit$lev)
fit$lev  # leveage = diagnal entries of the smoother matrix
diag(smooth.matrix(x, df =9))
fit$cv  # default: GCV
sum((y-fit$y)^2)/(1-fit$df/n)^2/n
fit=smooth.spline(x, y, df=9, cv=T) # set 'cv=T' to return CV 
fit$cv
sum(((y-fit$y)/(1-fit$lev))^2)/n
  # Use LOO-CV and GCV to select df.
  # Note that same as Ridge regression, smoothing splines could have fractional dfs.
df = 2+(1:40)/2
m = length(df)
mycv = rep(0,m)
mygcv = rep(0,m)
for (i in 1:m){
  fit = smooth.spline(x, y, df=df[i]);
  mygcv[i] = fit$cv;
  fit = smooth.spline(x, y, df=df[i], cv=T);
  mycv[i] = fit$cv
}

plot(c(1,20), range(mycv, mygcv)+c(-0.5,0.5), xlab='df', 
     ylab='CV scores', type='n')
points(df, mycv, col="red", cex=2);
points(df, mygcv, col="blue", cex=2);

optdf = df[mygcv==min(mygcv)]
optdf

fitopt = smooth.spline(x, y, df=optdf);
plot(x, y, xlab="x", ylab="y")
lines(predict(fitopt, (1:100)/100),col="red", lwd=2)
lines(fx, fy, col="gray", lwd=2)
