################################ Logistic Regression with Large n and/or Large p
# R is a nice environment for data exploration: we type a command, then we can 
# see the result or plots. But we pay a price for such convenience: R needs to 
# load the whole data into memory. For a large data set, either R cannot load the 
# whole data into memory (The data set 542 students need to analyze a couple of 
# years ago has over 45 million rows), or there isn't much memory left once the 
# data has been loaded in. 

# Big-something R Packages

# There are some bigxxx R packages, such as "bigmemory" and "biganalytics". You 
# can find some tutorials online. Those packages require the design matrix X to 
# be numerical, so dummy code your factor variables before calling those packages. 

# The rationale behind those bigxxx packages: load data batch by batch into R 
# when needed, to make sure there is still enough memory left for R to run its ordinary analysis. 

################################################### Code Your Own Newton-Raphson

# Instead of using those bigxxx packages, you can also recode logistic regression 
# when n is very large but p is moderate (e.g., p<=1000). Here by moderate, I mean 
# that we can still solve p linear equations in R. 

# In my Rcode below, I still keep a copy of X in memory, but when X is very large, 
# you can read in X[i,] (or a subset of rows of X) into R when needed. 

heart=read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data", 
sep=",", header=TRUE)
heart=heart[,-c(1,5,7)];
n=dim(heart)[1]; # data size
p=dim(heart)[2]; # dimension of the data
heart$famhist=as.numeric(heart$famhist)-1;

heartfull = glm(chd~., data=heart, family=binomial);
round(summary(heartfull)$coef, dig=3)

## Design matrix X (with intercept) and response Y (binary)
X = data.matrix(heart[, colnames(heart) != "chd"])
X = cbind(rep(1, n), X)

## If the original response is a factor variable, then you need
## to run Y = as.numeric(response.variable) - 1
Y = as.numeric(heart$chd)  

########################################
## initialize
########################################
## Initialize with the LS estimate of beta or beta=0
## mybeta = (X^t X)^{-1} X^t y, or equivalently
## (X^t X)beta = X^t y, i.e., newX*beta=newy, 
## where newX = X^t X and newy = Xy

newX = matrix(0, p, p); 
newY = rep(0, p);
for(i in 1:n){
newX = newX + X[i,] %*% t(X[i,])
newY = newY + Y[i]*X[i,]
}
beta = solve(newX, newY)
beta = as.vector(beta)
## check whether beta is the same as lm(Y ~ X-1)$coef

########################################
## Iteratively Run a Weighted LS
########################################
## beta.change = (X^t W X){-1} X^t W (y-p)

threshold = 0.01
num.iterations = 50;  ## maximum number of iterations
for(t in 1:num.iterations){
newX = matrix(0, p, p); 
newY = rep(0, p);
for(i in 1:n){
myp = 1/(1 + exp(-sum(X[i,]*beta)))
myw =myp*(1-myp)
newX = newX + myw*X[i,] %*% t(X[i,])
newY = newY + myw*(Y[i]-myp)*X[i,]
}
beta.change = solve(newX, newY)
beta.change = as.vector(beta.change)
if (max(abs(beta.change)) < threshold) break;
beta = beta + beta.change
}

########################################
## Output beta
########################################

round(beta, dig=3)
round(heartfull$coef, dig=3)

# When p is Large

# Newton-Raphson update requires to invert the Hessian matrix (that is a p-by-p 
# matrix for p-dimensional beta), so it's not desirable when p is very large. 

# When p is large, we can use gradient descent, i.e., compute the gradient at 
# the current value beta_0, and then move a small step (known as the learning rate) 
# toward the gradient (that is a p-dimensional direction). Regularization (L1 or L2) 
# can be incorporated into into this optimization framework easily. (L2 penalty 
# can be added to the above Newton-Raphson easily.) In addition, we do not need 
# to compute the gradient using all the data, but a random subset of the data, 
# known as stochastic gradient descent (SGD)

# You can check this note on stochastic gradient descent (SGD)
http://cseweb.ucsd.edu/~elkan/250B/logreg.pdf

# There are some sample R/python code for logistic regression using SGD. Google 
# "logistic regression SGD regularization r" or "logistic regression SGD 
# regularization python". You can also replace the term "regularization" with a 
# more specific term such as "L1".

