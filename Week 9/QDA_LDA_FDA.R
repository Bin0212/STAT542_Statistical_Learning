# Discriminant Analysis with R on Handwritten Digits Data
library(MASS)
#help(lda)
#help(qda)

# 40 samples from each class, totally 400 training data and 400 test data
load('digits.Rdata')

dim(X)
dim(Xtest)
table(Y)
table(Ytest)

# LDA
dig.lda = lda(X,Y);
Ytest.pred = predict(dig.lda, Xtest)$class
table(Ytest, Ytest.pred)

sum(Ytest != Ytest.pred)

# QDA
# Error message: the covariance matrix is not invertable. The MASS package does 
# not provide any solution for non-invertable covariance matrices for LDA or QDA.
dig.qda = qda(X,Y) # error message

# FDA
# The FDA direction is returned by the lda function. Use the directions to 
# project both the training and test data to a lower dimensional space.

# Caution: it is meaningless to show that classes are well-seperated on the 
# training data (same as showing training error is small); we need to check 
# seperation on the test data.

FDA.dir = dig.lda$scaling
dim(FDA.dir)  # at most 10-1 = 9 directions

F = X%*%FDA.dir;
par(mfrow=c(1,2))
plot(F[,1],F[,2], type="n", xlab="", ylab="");
text(F[,1], F[,2], Y, col=Y+1);

Ftest = Xtest%*%dig.lda$scaling;
plot(Ftest[,1], Ftest[,2], type="n", xlab="", ylab="");
text(Ftest[,1], Ftest[,2], Ytest, col=Ytest+1);

# You can plot the projections on other directions. None of them produces 
# meaningful results on the test data
xid = c(1,1,1,2); yid = c(2,3,4,3);
par(mfrow=c(2,2))
for(i in 1:4){
  plot(Ftest[,xid[i]], Ftest[,yid[i]], type="n", xlab="", ylab="");
  text(Ftest[,xid[i]], Ftest[,yid[i]], Ytest, col=Ytest+1);
}

# Check the calculation for FDA
# compute W (within group covariance matrix)
tmp.lm = lm(X ~ as.factor(Y)); # each x_i - its group mean
W = cov(tmp.lm$res)
eigenW = eigen(W)
summary(eigenW)
A = eigenW$ve %*% diag(1/sqrt(eigenW$va)) %*% t(eigenW$ve)

# compute B (between-group covariance matrix)
B = cov(tmp.lm$fitted)

tmp.eigen = eigen(A %*% B %*% t(A)) 
round(tmp.eigen$va, dig=5)  
# you'll find only the first 9 eigenvalues are non-zero.  

tmp = A %*% tmp.eigen$ve[, 1:9]

# The 9 directions computed by us (stored in tmp) are
# perfectly correlated with the ones returned by R
for(j in 1:9) print(cor(tmp[, j], FDA.dir[, j]))

# Compare the directions from FDA vs directions from PCA
Sigma = cov(X)
dig.eigen=eigen(Sigma);
PCA.dir = dig.eigen$ve[, 1:100]  # pick top 100 PCA directions
dim(PCA.dir)

# Note that directions from PCA are orthonormal, but directions from FDA are not.
colSums(FDA.dir^2)
colSums(PCA.dir^2)

round(t(FDA.dir) %*% FDA.dir, dig=5)
# just try try the first 10 directions
round(t(PCA.dir[, 1:10]) %*% PCA.dir[, 1:10], dig=5)  
round(t(FDA.dir) %*% W %*% FDA.dir, dig=5)

# Let's try the following: use PCA to reduce the dimension from 256 to 100,
# and then apply LDA. What's the accuracy?
newX = X %*% PCA.dir[, 1:100];
newdig.lda = lda(newX, Y);
newXtest = Xtest %*% PCA.dir[, 1:100]
Ytest.pred = predict(newdig.lda, newXtest)$class
table(Ytest, Ytest.pred)

sum(Ytest != Ytest.pred)

# Even the projects on the FDA directions look better if we pre-cut the dimension 
# from 256 to the first 100 PCs.
newF=newX%*%newdig.lda$scaling;
newFtest=Xtest%*%dig.eigen$ve[,1:100]%*%newdig.lda$scaling

par(mfrow=c(1,2));
plot(newF[,1],newF[,2], type="n", xlab="", ylab="")
text(newF[,1], newF[,2], Y, col= Y+1)
plot(newFtest[,1], newFtest[,2], type="n", xlab="", ylab="")
text(newFtest[,1], newFtest[,2], Ytest, col=Ytest+1)
