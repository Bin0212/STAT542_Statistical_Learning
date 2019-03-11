# include all libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "glmnet",
  "xgboost",
  "mgcv"
)

# set seed for reproducibility 
set.seed(8259)

# read in data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# LASSO  ---------------------------------
# preprocess data
source("DataPrepLASSO.R")
train.LASSO <- train
test.LASSO <- test
data.LASSO <- DataPrepLASSO(train.LASSO, test.LASSO)
train.LASSO <- data.LASSO$train
test.LASSO <- data.LASSO$test
train.LASSO.y <- data.LASSO$train.y

# perform cross validation on LASSO
cv.out <- cv.glmnet(train.LASSO, train.LASSO.y, alpha = 1)
# perform prediction on LASSO with lambda.min
tmp.LASSO <-predict(cv.out, s = cv.out$lambda.min, newx = test.LASSO)

# write to mysubmission1
write.table(cbind(test[,'PID'] ,exp(tmp.LASSO)), file = "mysubmission1.txt", 
            sep = ", ", row.names = FALSE,col.names = c('PID', 'Sale_Price'), 
            quote = FALSE)


# BOOSTING TREE  --------------------------
# preprocess data
source("DataPrepBoost.R")
train.boost <- train
test.boost <- test
data.boost <- DataPrepBoost(train.boost, test.boost)
train.boost <- data.boost$train
test.boost.x <- data.boost$test
remove.var <- c('Sale_Price')
train.boost.y <- log(train.boost$Sale_Price)
train.boost.x <- train.boost[,!colnames(train.boost) %in% remove.var]

# set parameters
params <- list(
    colsample_bytree=0.2,
    gamma=0.0,
    eta=0.01,
    max_depth=4,
    min_child_weight=1.5,
    alpha=0.9,
    lambda=0.6,
    subsample=0.2,
    seed=8259
)

# run xgboost
xgb.model <- xgboost(data = as.matrix(train.boost.x), label = as.matrix(train.boost.y),
                     nrounds = 10000, params = params, verbose = FALSE)
tmp.boost <- predict(xgb.model, as.matrix(test.boost.x))

write.table(cbind(test[,'PID'] ,exp(tmp.boost)), file = "mysubmission2.txt", 
            sep = ", ", row.names = FALSE,col.names = c('PID', 'Sale_Price'), 
            quote = FALSE)


# GAM  ------------------------------------
source("DataPrepGAM.R")
train.GAM <- train
test.GAM <- test
data.GAM <- DataPrepGAM(train.GAM, test.GAM)
train.GAM <- data.GAM$train
test.GAM <- data.GAM$test
select.binary.vars <- data.GAM$select.level.var
linear.vars <- data.GAM$linear.vars
num.vars <- data.GAM$num.vars

# create formula
gam.formula <- paste0("Sale_Price ~ ", linear.vars[1])
for(var in c(linear.vars[-1], select.binary.vars)){
  gam.formula <- paste0(gam.formula, " + ", var)
}
for(var in num.vars){
  gam.formula <- paste0(gam.formula, " + s(", var, ")")
}
gam.formula <- as.formula(gam.formula)

# run GAM
gam.model <- gam(gam.formula, data = train.GAM, method="REML")
tmp.GAM <- predict.gam(gam.model, newdata = test.GAM)
write.table(cbind(test[,'PID'] ,exp(tmp.GAM)), file = "mysubmission3.txt", 
            sep = ", ", row.names = FALSE,col.names = c('PID', 'Sale_Price'), 
            quote = FALSE)

