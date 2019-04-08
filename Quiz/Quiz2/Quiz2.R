#set to working directory
setwd("/Users/bin/Documents/2_UIUC/8_2019_Spring/STAT 542 Statistical Learning/Quiz/Quiz2")

x <- read.csv("train_housing_quiz2.csv")
nrow(x)
ncol(x)

min(x[,81])
max(x[,81])
median(x[,81])

sum(x$SalePrice > 500000)

allvals<-as.character(x[,17])
valslist<-sort(unique(allvals))

LRModel <- lm(log(x$SalePrice+1) ~ x$OverallQual + I(log(x$X1stFlrSF+1)) + 
                I(log(x$GrLivArea+1)) + x$GarageCars + I(log(x$GarageArea + 1)))

summary(LRModel)

logprice <- 7.284606 + 0.134437*6 + 0.213215*log(1087+1) + 0.305150*log(1464+1) + 0.097866*2 + 0.005895*log(480+1)
price <- exp(logprice)

allvals<-as.character(x$GarageCars)
valslist<-sort(unique(allvals))

LRModel_c <- lm(log(x$SalePrice+1) ~ x$OverallQual + I(log(x$X1stFlrSF+1)) + 
                I(log(x$GrLivArea+1)) + factor(x$GarageCars) + I(log(x$GarageArea + 1)))

anova(LRModel, LRModel_c)