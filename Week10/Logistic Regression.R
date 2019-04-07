######################################### Convergence Issue for a Simple Example
# Consider the following simple example: Y = 1 if x1 + x2 > 0, Y=0, otherwise.

x = matrix(runif(20), 10, 2)
y = ifelse(x[,1]+x[,2]>1, 1, 0); 
plot(x[,1], x[,2], type="n", xlab="", ylab=""); 
text(x[,1], x[,2], y )

myfit=glm(y~x, family=binomial)
summary(myfit)

######## The likelihood for logistic function doesn't converge when the data are 
# well-separated: the MLE of beta would converge to infinity or negative-infinity. 
# However, the separating line is well-defined and we can still use the fitted model 
# to do prediction.

# South African Heart Disease Data
# The Coronary Risk‐Factor Study data involve 462 males between the ages of 15 
# and 64 from a heart‐disease high‐risk region of the Western Cape, South Africa. 
# The response is "chd", the presence (chd=1) or absence (chd=0) of coronary heart disease.

# There are 9 covariates:
  
# sbp - systolic blood pressure
# tobacco - cumulative tobacco (kg)
# ldl - low densiity lipoprotein cholesterol
# adiposity
# famhist - family history of heart disease (Present, Absent)
# typea - type‐A behavior
# obesity
# alcohol - current alcohol consumption
# age - age at onset

# Fit a logistic model
heart = read.table("https://web.stanford.edu/~hastie/ElemStatLearn//datasets/SAheart.data", 
                   sep=",",head=T, row.names=1)
head(heart)

heartfull = glm(chd~., data=heart, family=binomial)
summary(heartfull)

# How to interprete the coefficient associated with variable “obesity”? It is 
# strange that having excessive body fat can actually lower your chance of having 
# a heart disease?

round(cor(data.matrix(heart)), dig=2)

# What’s null deviance?
# What’s residual deviance?
phat=heartfull$fitted.values;
-2*(sum(log(phat[heart$chd==1])) + sum(log(1-phat[heart$chd==0])))

# Stepwise Mode Selection with AIC and BIC.
# The two procedures happen to return the same model.

heartstepA = step(heartfull, scope=list(upper=~., lower=~1))
summary(heartstepA)

n=dim(heart)[1]
# trace = 0 means not showing the intermediate steps, k= log(n) for BIC
heartstepB = step(heartfull, scope=list(upper=~., lower=~1), trace = 0, k=log(n))
summary(heartstepB)

# Estimation/Prediction
# Estimation (probability of being 1) on the training samples.
phat=heartstepA$fitted.values;
mypred = (phat>0.5)
table(heart$chd, mypred)

# Consider the following three males with the same value on all other predictors 
# except age: min, max, and median. What’s the estimated chance of getting heart 
# disease for each of them?
testsamples = heart[c(1, 1, 1),]
testsamples
testsamples$age = c(min(heart$age), median(heart$age), max(heart$age))
testsamples

# predict log-odds
# predict probabilities
predict(heartstepA, newdata=testsamples)
predict(heartstepA, newdata=testsamples, type="response")


############################## Challenger USA Space Shuttle O-Ring Data Analysis
# The motivation for collecting this database was the explosion of the USA Space 
# Shuttle Challenger on 28 January 1986. The explosion was eventually traced to 
# the failure of one of the three field joints on one of the two solid booster rockets. 
# Each of these six field joints includes two O‐rings, designated as primary and secondary, 
# which fail when phenomena called erosion and blowby both occur.

# The night before the launch a decision had to be made regarding launch safety. 
# The discussion among engineers and managers leading to this decision included 
# concern that the probability of failure of the O‐rings depended on the temperature 
# at launch, which was forecasted to be 31 degrees F. There are strong engineering 
# reasons based on the composition of O‐rings to support the judgment that failure 
# probability may rise monotonically as temperature drops. One other variable, the 
# pressure at which safety testing for field join leaks was performed, was available, 
# but its relevance to the failure process was unclear.

# The data set includes the temperature and the number of O‐ring failures for the 
# 23 shuttle flights previous to the Challenger disaster. No previous liftoff temperature 
# was below 53 degrees F. So tremendous extrapolation must be done from the given 
# data to assess risk at 31 degrees F. However it is obvious (from the plot) the risk 
# is high at 31 degrees F.

# The task is to predict the Chance of one of the O‐rings would fail when the launch 
# temperature is below freezing.
orings = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/space-shuttle/o-ring-erosion-or-blowby.data")
orings

orings=orings[, c(2:3)]
names(orings)=c("damage", "temp")
orings[order(orings$temp),]

logitmod=glm(cbind(damage, 6-damage) ~ temp, family=binomial, data=orings)
summary(logitmod)

predict(logitmod,  data.frame(temp=31), type="response")

min.temp=31; max.temp=max(orings$temp)
plot(orings$temp, orings$damage/6, 
     xlim=c(min.temp, max.temp), ylim=c(0,1), 
     xlab="Temp", ylab="Chance of Damage")
newtemp = seq(min.temp, max.temp, length=100)
phat = predict(logitmod, data.frame(temp=newtemp), type="response")
lines(newtemp, phat, col="red")

#################################################### Logistic with Lasso penalty
library(glmnet)

p = dim(heart)[2]
X=data.matrix(heart[,-p]);
Y=heart[,p];
heart.l1=glmnet(X,Y,family="binomial",alpha=1)
plot(heart.l1,label=TRUE)

heart.cv = cv.glmnet(X, Y, family="binomial",alpha=1)
plot(heart.cv)

heart.cv$lambda.min
heart.cv$lambda.1se
predict(heart.cv, lambda=heart.cv$lambda.1se, type="coefficients")
