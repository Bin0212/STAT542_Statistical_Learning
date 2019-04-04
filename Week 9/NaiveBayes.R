library(klaR)
library("ElemStatLearn")
names(spam)[ncol(spam)]="Y"
spam$Y = ifelse(spam$Y=="spam", 1, 0)
spam$Y = as.factor(spam$Y)

# Try NaiveBayes in package klaR on the Spam data. Use the whole data as training 
# and compute the training error.

NBfit = NaiveBayes( Y ~ ., data= spam)

names(NBfit)   # check the output from NaiveBayes

# Estimated parameters are
# 1) class frequency 2x1
NBfit$apriori  
# 2) class-specific mean/var for each class and each of the 57 features
NBfit$tables

y.class = predict(NBfit, newdata=spam)$class
y.prob = predict(NBfit, newdata = spam)$post
y.prob = y.prob[,1]  # prob of in class 1
table(spam$Y, y.prob>0.5)
# There are 50 warning messages. The maximum number of warning messages is 
# often set to be 50, so it's possible that there are more than 50 warnings. 
# It seems all the warning messages are the same, "Numerical 0 probability for 
# all classes with observation 1."

# Let's find out why there are warning messages. You can use the command 
# getAnywhere to show the source code of predict.NaiveBayes
getAnywhere(predict.NaiveBayes)

# Check where the warning message is located, then you'll find out that this 
# message is printed because when computing the prediction, R thinks  sum(exp(L)) 
# is very close to 0.

# What's L? If we have two classes, 1 or 2, then for each test sample, L has 
# two elements where
  # L1 = log P(x | Y=1) + log P(Y=1) and
  # L2 = log P(x | Y=2) + log P(Y=2).

# The posterior probablity is equal to P(Y=1 | x) = exp(L1)/(exp(L1) + exp(L2)).
# Recall that P(x|Y) is a product of one-dimensional normal densities; 
# in predict.NaiveBayes, each normal density is evaluated using R function  dnorm. 
# It's possible that the density functions for most dimensions are less than 1, 
# then their log values are negative. If the dimension p is large, then L1 or L2, 
# which is a sum of many negative values, could be easily less than -20, but in R, 
# exp(-20) is regarded as 0.
isTRUE(all.equal(exp(-20), 0))

# Next let's code the prediction differently to avoid the warning messages.
  # compute the normal parameters for each dimension and each class using training 
  # data: group means and standard deviations are saved in  mymean and mysd, 
  # two 2-by-p matrices, which should be the same as NBfit$tables returned by NaiveBayes.
trainX = spam[, -58]; trainY = spam$Y;
p=dim(trainX)[2]; # p: number of predictors
y.levels = levels(trainY)
K= length(y.levels) # number of groups
mymean = matrix(0, K, p)  
mysd = matrix(0, K, p)    

for(k in 1:K){
  mymean[k,] = apply(trainX[trainY == y.levels[k],], 2, mean)
  mysd[k,] = apply(trainX[trainY == y.levels[k],], 2, sd)
}
w=mean(trainY==y.levels[1])

  # How does predict.NaiveBayes compute the posterior probability for class 1? 
  # To compute the log density for each dimension,  predict.NaiveBayes 1) calls 
  # dnorm to evaluate the density; 2) if density is too small, say <0.001, set it 
  # to be 0.001; 3) take log of the density. Such a procedure may lead to some 
  # computation error, simply because log(exp(-20)) is NOT equal to -20, but -7, 
  # since the code will set exp(-20) to be 0.001 and log(0.001) is about -7.

ntrain = length(trainY)
tmp1 = rep(0, ntrain); tmp2=rep(0, ntrain)
for(i in 1:p){
  prob = dnorm(trainX[,i], mymean[1,i], mysd[1,i])
  prob[prob==0] = 0.001
  tmp1 = tmp1 + log(prob)
  prob = dnorm(trainX[,i], mymean[2,i], mysd[2,i])
  prob[prob==0] = 0.001
  tmp2 = tmp2 + log(prob)
}
tmp1 = tmp1 + log(w)
tmp2 = tmp2 + log(1-w)
my.prob = 1/(1 + exp(tmp2-tmp1))  

# my.prob should be the same as y.prob
which(abs(my.prob - y.prob) > 0.001)  
table(my.prob>0.5, y.prob>0.5)  

# Instead of calling dnorm to compute the density and then take log, we directly 
# evaluate the log of the density function, which is a quadratic function of X's. 
# You'll find that the prediction from our calculation is different from the one 
# from NaiveBayes.
newtmp1 = rep(0, ntrain); newtmp2=rep(0, ntrain)
for(i in 1:p){
  newtmp1 = newtmp1 - log(mysd[1,i]) - (trainX[,i] - mymean[1,i])^2/(2*mysd[1,i]^2)
  newtmp2 = newtmp2 - log(mysd[2,i]) - (trainX[,i] - mymean[2,i])^2/(2*mysd[2,i]^2)
}
newtmp1 = newtmp1 + log(w)
newtmp2 = newtmp2 + log(1-w)
diff = newtmp2-newtmp1
my.newprob = 1/(1 + exp(diff)) 
table(my.newprob>0.5, my.prob>0.5)  

# Let’s find out on which samples our prediction differs from NaiveBayes. 
# Take a look of the 177th example: the prediction from  NaiveBayes is class 1, 
# but the prediction from direct calculation (my.newprob) is class 0.
id = which((my.newprob>0.5) != (my.prob>0.5))
j=id[1];  # the 177th observation

# different prediction
my.newprob[177]
my.prob[177]

# Evaluate the log-density for class 1. You’ll find that at dim 20, this 
# particular example is at the tail of the normal, so its density is very 
# small (or equivalently, -log is very large), but the default NaiveBayes 
# truncates that contribution. So this is why the NaiveBayes predicts it to be 
# in class 1, but the direct computation predicts it to be class 0.

# junk[,1]: dnorm, threshold, and then log
# junk[,2]: directly evaluate the log-density
k=1;
junk=matrix(0, p, 2)
for(i in 1:p){
  prob = dnorm(trainX[j,i], mymean[k,i], mysd[k,i])
  if (prob==0) {prob = 0.001}
  junk[i,1]=log(prob)
  junk[i, 2]= -0.5*log(2*pi)-log(mysd[k,i]) - (trainX[j,i]-mymean[k,i])^2/(2*mysd[k,i]^2)
}
round(junk, dig=2)[1:25,]
