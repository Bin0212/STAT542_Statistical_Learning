############### Data Preparation ###############
csize = 10;       # number of centers
p = 2;      
s = 1;      # sd for generating the centers within each class                    
m1 = matrix(rnorm(csize*p), csize, p)*s + cbind( rep(1,csize), rep(0,csize));
m0 = matrix(rnorm(csize*p), csize, p)*s + cbind( rep(0,csize), rep(1,csize));
#Generating training data
n=100;  
# Randomly allocate the n samples for class 1  to the 10 clusters
id1 = sample(1:csize, n, replace = TRUE);
# Randomly allocate the n samples for class 1 to the 10 clusters
id0 = sample(1:csize, n, replace = TRUE);  

s= sqrt(1/5);                               # sd for generating x. 

traindata = matrix(rnorm(2*n*p), 2*n, p)*s + rbind(m1[id1,], m0[id0,])
Ytrain = factor(c(rep(1,n), rep(0,n)))
#Generating test data
N = 5000;  
id1 = sample(1:csize, N, replace=TRUE);
id0 = sample(1:csize, N, replace=TRUE); 
testdata = matrix(rnorm(2*N*p), 2*N, p)*s + 
  rbind(m1[id1,], m0[id0,])
Ytest = factor(c(rep(1,N), rep(0,N)))
###################### End ##################### 

################# Visualization ################
plot(traindata[, 1], traindata[, 2], type = "n", xlab = "", ylab = "")

points(traindata[1:n, 1], traindata[1:n, 2], col = "blue");
points(traindata[(n+1):(2*n), 1], traindata[(n+1):(2*n), 2], col="red"); 

points(m1[1:csize, 1], m1[1:csize, 2], pch="+", cex=1.5, col="blue");    
points(m0[1:csize, 1], m0[1:csize, 2], pch="+", cex=1.5, col="red");   

legend("bottomleft", pch = c(1,1), col = c("red", "blue"), 
       legend = c("class 1", "class 0"))
###################### End ##################### 

################# Bayes Error ################## 
mixnorm=function(x){
  ## return the density ratio for a point x, where each 
  ## density is a mixture of normal with 10 components
  sum(exp(-apply((t(m1)-x)^2, 2, sum)*5/2))/sum(exp(-apply((t(m0)-x)^2, 2, sum)*5/2))
}

Ytest_pred_Bayes = apply(testdata, 1, mixnorm)
Ytest_pred_Bayes = as.numeric(Ytest_pred_Bayes > 1);
table(Ytest, Ytest_pred_Bayes); 
test.err.Bayes = sum(Ytest !=  Ytest_pred_Bayes) / (2*N)
###################### End ##################### 
