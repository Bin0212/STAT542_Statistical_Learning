#In the following code, we generate Y from the same linear regression model, 
#but X is generated from uniform(-1,1) in case 1, and from uniform(-6, -5) and 
#uniform(5, 6) in case 2. The resulting R-square is 16% for case 1 and 97% for case 2. 
set.seed(2500)
n = 200
X1 = runif(n, -1, 1)
Y = X1 + rnorm(n)
fit = lm(Y ~ X1)
summary(fit)

newX1 = X1
id = which(X1 < 0)
newX1[id] = newX1[id] - 5; 
newX1[-id] = newX1[-id] + 5; 
newY = newX1 + rnorm(n)
newfit = lm(newY ~ newX1)
summary(newfit)

#Plot the data and fitted lines
plot(c(-6, 6), c(-7, 7), type="n")
points(X1, Y, col = "blue", pch=".")
abline(coef(fit), col="blue")
points(newX1, newY, col = "red", pch=".")
abline(coef(newfit), col="red")