library(MASS)  
data(Boston)  

names(Boston)

fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(fit)
sum(fit$residuals^2)
predict(fit, data.frame(dis=6))

fit2 = lm(nox ~ poly(dis, 4), data = Boston)
summary(fit2)
sum(fit2$residuals^2)
predict(fit2, data.frame(dis=6))

lm(nox ~ bs(dis, df=3), data=Boston)
# options
lm(nox ~ bs(dis, df= 4, intercept=TRUE), data=Boston) 
lm(nox ~ poly(dis, 3), data=Boston)  
lm(nox ~ bs(dis, df= 5, intercept=TRUE), data=Boston)  
lm(nox ~ bs(dis, knots=median(dis)), data=Boston)  
lm(nox ~ bs(dis, knots=quantile(dis, prob=c(0.25, 0.5, 0.75))), data=Boston)
   
   