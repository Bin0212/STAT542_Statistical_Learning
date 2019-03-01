X = read.table("YourImage copy.txt"); 
X = as.matrix(X); 
X = t(X[225:1,]); 
image(X, col=gray((0:128)/128)); 

# divide image into 6x6 patches
# save the patches (36x1 vector) into data matrix Z
p = 36
Z = NULL 
n = (225*225)/36
for(i in 1:(225/6))
  for(j in 1:(225/6)){
    k=6*(i-1); l=6*(j-1);
    Z=rbind(Z,as.vector(X[(k+1):(k+6), (l+1):(l+6)]));
  }
dim(Z)  # 2700x36 

# run k-means with K=40 centers
mykm = kmeans(Z, centers=40, nstart=10)

# Use the 40 patches to replace 2700 patches in the original image
# The new image uses only 40 disctive patches
newX = matrix(0,225,225)
m = 0
for(i in 1:(225/6))
  for(j in 1:(225/6)){
    k = 6*(i-1)
    l = 6*(j-1)
    m = m+1;
    tmp = mykm$center[mykm$cluster[m],]
    newX[(k+1):(k+6), (l+1):(l+6)] = matrix(tmp,6,6);
  }
image(newX, col=gray((0:128)/128))
