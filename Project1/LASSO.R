LASSO = function(train, test){
  require("glmnet")
  # remove factor variables
  categorical.vars <- colnames(train)[which(sapply(train,
                                                   function(x) is.factor(x)))]
  train.LASSO <- model.matrix(~., train[,!colnames(train) 
                                        %in% categorical.vars])[,-1]
  test.LASSO <- model.matrix(~., test[,!colnames(test) 
                                      %in% categorical.vars][,-1])
  cv.LASSO <- cv.glmnet(train.LASSO[,2:dim(train.LASSO)[2] - 1], 
                        log(train.LASSO[,dim(train.LASSO)[2]]), alpha=1)
  fit.LASSO <- glmnet(train.LASSO[,2:dim(train.LASSO)[2] - 1], 
                      log(train.LASSO[,dim(train.LASSO)[2]]), 
                      lambda = cv.LASSO$lambda.min, alpha = 1)
  predict.LASSO <- predict(fit.LASSO, s = cv.LASSO$lambda.min, newx = test.LASSO)
  exp(predict.LASSO)
}
