# set working directory
setwd("~/Documents/2_UIUC/8_2019_Spring/STAT 542 Statistical Learning/Project/P1")

# load data and train/test sample row index
data <- read.csv("Ames_data.csv")
load("project1_testIDs.Rdata")

for (i in 1:10){
  # remove(list = ls())
  #data <- read.csv("Ames_data.csv")
  #load("project1_testIDs.Rdata")
  # generate data
  source("DataGenerate.R")
  DataGenerate(i)
  
  # read in data
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  test.y <- read.csv("test_y.csv")
  test.y <- log(test.y$Sale_Price)
  
  # LASSO  ---------------------------------
  # preprocess data
  source("DataPrepLASSO.R")
  train.LASSO <- train
  test.LASSO <- test
  data.LASSO <- DataPrepLASSO(train.LASSO, test.LASSO)
  train.LASSO <- data.LASSO$train
  test.LASSO <- data.LASSO$test
  
  train.LASSO.y <- data.LASSO$train.y
  set.seed(100)
  cv.out <- cv.glmnet(train.LASSO, train.LASSO.y, alpha = 1)
  tmp <-predict(cv.out, s = cv.out$lambda.min, newx = test.LASSO)
  #??? have "" in col.names
  write.table(tmp, file = "mysubmission1.txt", sep = ", ", row.names = FALSE,
              col.names = TRUE)
  rmse <- sqrt(mean((tmp - test.y)^2))
  print(rmse)

  # GAM  ------------------------------------
  require("mgcv")
  
  fm <- lapply(dimnames(train.LASSO)[2], function(x) 
    paste('s(`', x, '`)', sep = "", collapse = ' + '))
  fm <- as.formula(paste('y ~', fm))
  
  train.GAM <- gam(fm, data = as.data.frame(train.LASSO))
  tmp <- predict.gam(train.GAM, newdata = test.LASSO)
  # # BOOSTING TREE  --------------------------
  # # preprocess data
  # source("DataPrepBoost.R")
  # train.boost <- train
  # test.boost <- test
  # data.boost <- DataPrepBoost(train.boost, test.boost)
  # train.boost <- data.boost$train
  # test.boost <- data.boost$test
  # 
  # # ? somehow PID has a positive effect on prediction accuracy
  # remove.var <- c('Sale_Price')
  # train.boost.y <- log(train.boost$Sale_Price)
  # train.boost.x <- train.boost[,!colnames(train.boost) %in% remove.var]
  # test.boost.x <- test.boost[,!colnames(test.boost) %in% remove.var]
  # 
  # require("xgboost")
  # params <- list(
  #     colsample_bytree=0.2,
  #     gamma=0.0,
  #     eta=0.01,
  #     max_depth=4,
  #     min_child_weight=1.5,
  #     alpha=0.9,
  #     lambda=0.6,
  #     subsample=0.2,
  #     seed=8259
  # )
  # 
  # xgb.model <- xgboost(data = as.matrix(train.boost.x), label = as.matrix(train.boost.y),
  #                      nrounds = 5000, params = params, verbose = 0)
  # tmp <- predict(xgb.model, as.matrix(test.boost.x))
  # write.table(tmp, file = "mysubmission2.txt", sep = ", ", row.names = FALSE,
  #             col.names = TRUE)
  # rmse <- sqrt(mean((tmp - test.y)^2))
  # print(rmse)

  # GAM  ------------------------------------
  
}

## results for xgboost w/ 5000 rounds
# [1] 0.1213035
# [1] 0.120166
# [1] 0.1211247
# [1] 0.1154751
# [1] 0.1116195
# [1] 0.1308286
# [1] 0.1301483
# [1] 0.1290884
# [1] 0.128732
# [1] 0.1225521

## results for xgboost w/ 10000 rounds
# [1] 0.1191841
# [1] 0.1206038
# [1] 0.1208109
# [1] 0.1161768
# [1] 0.1108303
# [1] 0.1299468
# [1] 0.129092
# [1] 0.1280239
# [1] 0.1269553
# [1] 0.1203112

## results for xgboost w/ 5000 rounds wo/ PID
# [1] 0.1213605
# [1] 0.1192709
# [1] 0.1206916
# [1] 0.1172265
# [1] 0.1119317
# [1] 0.1323279
# [1] 0.1310615
# [1] 0.1300588
# [1] 0.1287685
# [1] 0.1232081

# # results for xgboost w/ 5000 rounds w/ PID
# [1] 0.1196024
# [1] 0.1196312
# [1] 0.1198901
# [1] 0.1161898
# [1] 0.1126608
# [1] 0.1324853
# [1] 0.1299463
# [1] 0.1282268
# [1] 0.128678
# [1] 0.121617

# # results for LASSO 
# [1] 0.1276809
# [1] 0.120503
# [1] 0.1420143
# [1] 0.1386641
# [1] 0.1201118
# [1] 0.1331143
# [1] 0.1289814
# [1] 0.1212651
# [1] 0.1349361
# [1] 0.1249204