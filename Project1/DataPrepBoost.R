DataPrepBoost = function(train, test){
  # Detect wihch column has NA values
  na.name.train <- sapply(names(train), function(x) length(which(is.na(train[, x]))))
  cname.train <- which(na.name.train > 0)  # 60th col: Garage_Yr_Blt
  na.name.test <- sapply(names(test), function(x) length(which(is.na(test[, x]))))
  cname.test <- which(na.name.test > 0)  # 60th col: Garage_Yr_Blt
  
  # Replace NA values with 0
  for (n in cname.train){
    na.index.train <- which((is.na(train[, n])))
    train[na.index.train, n] <- 0
  }
  for (n in cname.test){
    na.index.test <- which((is.na(test[, n])))
    test[na.index.test, n] <- 0
  }
  
  # Drop Gr_Live_Area > 4000
  drop.index <- which(train$Gr_Liv_Area>4000)
  train <- train[-drop.index,]
  
  # combine numerical data 
  categorical.vars <- colnames(train)[which(sapply(train, 
                                                        function(x) is.factor(x)))]
  train.matrix <- train[, !colnames(train) %in% categorical.vars, drop=FALSE]
  test.matrix <- test[, !colnames(test) %in% categorical.vars, drop=FALSE]
  
  # combine quality meansurements as numerical values
  map.overall_qual <- c("Very_Poor" = 1, "Poor" = 1, "Below_Average" = 1, "Fair" = 1, 
                       "Average" = 2, "Above_Average" = 2, "Good" = 3, 
                       "Very_Good" = 3, "Excellent" = 3, "Very_Excellent" = 3)
  Overall_Qual <- map.overall_qual[as.character(train$Overall_Qual)]
  train.matrix <- cbind(Overall_Qual, train.matrix)
  Overall_Qual <- map.overall_qual[as.character(test$Overall_Qual)]
  test.matrix <- cbind(Overall_Qual, test.matrix)
  
  map.exter_qual <- c("Fair" = 1, "Typical" = 2, "Good" = 3, "Excellent" = 4)
  Exter_Qual <- map.exter_qual[as.character(train$Exter_Qual)]
  train.matrix <- cbind(Exter_Qual, train.matrix)
  Exter_Qual <- map.exter_qual[as.character(test$Exter_Qual)]
  test.matrix <- cbind(Exter_Qual, test.matrix)
  
  map.bsmt_qual <- c("No_Basement" = 0, "Poor" = 1, "Fair" = 1, "Typical" = 2, 
                     "Good" = 3, "Excellent" = 4)
  Bsmt_Qual <- map.bsmt_qual[as.character(train$Bsmt_Qual)]
  train.matrix <- cbind(Bsmt_Qual, train.matrix)
  Bsmt_Qual <- map.exter_qual[as.character(test$Bsmt_Qual)]
  test.matrix <- cbind(Bsmt_Qual, test.matrix)
  
  map.heating_qc <- c("Poor" = 1, "Fair" = 1, "Typical" = 2, 
                      "Good" = 3, "Excellent" = 4)
  Heating_QC <- map.heating_qc[as.character(train$Heating_QC)]
  train.matrix <- cbind(Heating_QC, train.matrix)
  Heating_QC <- map.heating_qc[as.character(test$Heating_QC)]
  test.matrix <- cbind(Heating_QC, test.matrix)
  
  map.bsmt_cond <- c("No_Basement" = 0, "Poor" = 1, "Fair" = 1, "Typical" = 2, 
                     "Good" = 3, "Excellent" = 4)
  Bsmt_Cond <- map.bsmt_cond[as.character(train$Bsmt_Cond)]
  train.matrix <- cbind(Bsmt_Cond, train.matrix)
  Bsmt_Cond <- map.bsmt_cond[as.character(test$Bsmt_Cond)]
  test.matrix <- cbind(Bsmt_Cond, test.matrix)
  
  map.kitchen_qual <- c("Poor" = 1, "Fair" = 1, "Typical" = 2, 
                     "Good" = 3, "Excellent" = 4)
  Kitchen_Qual <- map.kitchen_qual[as.character(train$Kitchen_Qual)]
  train.matrix <- cbind(Kitchen_Qual, train.matrix)
  Kitchen_Qual <- map.kitchen_qual[as.character(test$Kitchen_Qual)]
  test.matrix <- cbind(Kitchen_Qual, test.matrix)
  
  map.fireplace_qu <- c("No_Fireplace" = 0, "Poor" = 1, "Fair" = 1, 
                        "Typical" = 2, "Good" = 3, "Excellent" = 4)
  Fireplace_Qu <- map.fireplace_qu[as.character(train$Fireplace_Qu)]
  train.matrix <- cbind(Fireplace_Qu, train.matrix)
  Fireplace_Qu <- map.fireplace_qu[as.character(test$Fireplace_Qu)]
  test.matrix <- cbind(Fireplace_Qu, test.matrix)
  
  map.garage_qual <- c("No_Garage" = 0, "Poor" = 1, "Fair" = 1, 
                        "Typical" = 2, "Good" = 3, "Excellent" = 4)
  Garage_Qual <- map.garage_qual[as.character(train$Garage_Qual)]
  train.matrix <- cbind(Garage_Qual, train.matrix)
  Garage_Qual <- map.garage_qual[as.character(test$Garage_Qual)]
  test.matrix <- cbind(Garage_Qual, test.matrix)
  
  map.garage_cond <- c("No_Garage" = 0, "Poor" = 1, "Fair" = 1, 
                       "Typical" = 2, "Good" = 3, "Excellent" = 3)
  Garage_Cond <- map.garage_cond[as.character(train$Garage_Cond)]
  train.matrix <- cbind(Garage_Cond, train.matrix)
  Garage_Cond <- map.garage_cond[as.character(test$Garage_Cond)]
  test.matrix <- cbind(Garage_Cond, test.matrix)
  
  map.bsmt_exposure <- c("No_Basement" = 0, "No" = 1, "Mn" = 2, "Av" = 3, "Gd" = 4)
  Bsmt_Exposure <- map.bsmt_exposure[as.character(train$Bsmt_Exposure)]
  train.matrix <- cbind(Bsmt_Exposure, train.matrix)
  Bsmt_Exposure <- map.bsmt_exposure[as.character(test$Bsmt_Exposure)]
  test.matrix <- cbind(Bsmt_Exposure, test.matrix)
  
  map.bsmtfin_type <- c("No_Basement" = 0, "Unf" = 1, "LwQ" = 2, "Rec" = 3, 
                          "BLQ" = 4, "ALQ" = 5, "GLQ" = 6)
  BsmtFin_Type_1 <- map.bsmtfin_type[as.character(train$BsmtFin_Type_1)]
  train.matrix <- cbind(BsmtFin_Type_1, train.matrix)
  BsmtFin_Type_1 <- map.bsmtfin_type[as.character(test$BsmtFin_Type_1)]
  test.matrix <- cbind(BsmtFin_Type_1, test.matrix)
  BsmtFin_Type_2 <- map.bsmtfin_type[as.character(train$BsmtFin_Type_2)]
  train.matrix <- cbind(BsmtFin_Type_2, train.matrix)
  BsmtFin_Type_2 <- map.bsmtfin_type[as.character(test$BsmtFin_Type_2)]
  test.matrix <- cbind(BsmtFin_Type_2, test.matrix)
  
  map.functional <- c("Sal" = 1, "Sev" = 2, "Maj2" = 3, "Maj1" = 4, 
                      "Mod" = 5, "Min2" = 6, "Min1" = 7, "Typ" = 8)
  Functional <- map.functional[as.character(train$Functional)]
  train.matrix <- cbind(Functional, train.matrix)
  Functional <- map.functional[as.character(test$Functional)]
  test.matrix <- cbind(Functional, test.matrix)
  
  map.garage_finish <- c("No_Garage" = 0, "Unf" = 1, "RFn" = 2, "Fin" = 3)
  Garage_Finish <- map.garage_finish[as.character(train$Garage_Finish)]
  train.matrix <- cbind(Garage_Finish, train.matrix)
  Garage_Finish <- map.garage_finish[as.character(test$Garage_Finish)]
  test.matrix <- cbind(Garage_Finish, test.matrix)
  
  map.fence <- c("No_Fence" = 0, "Minimum_Wood_Wire" = 1, "Good_Wood" = 2, 
                 "Minimum_Privacy" = 3, 'Good_Privacy' = 4)
  Fence <- map.fence[as.character(train$Fence)]
  train.matrix <- cbind(Fence, train.matrix)
  Fence <- map.fence[as.character(test$Fence)]
  test.matrix <- cbind(Fence, test.matrix)
  
  map.sale_condition <- c('Abnorml' = 1, 'Alloca' = 1, 'AdjLand' = 1, 'Family' = 1, 
                          'Normal' = 0, 'Partial' = 0)
  Sale_Condition <- map.sale_condition[as.character(train$Sale_Condition)]
  train.matrix <- cbind(Sale_Condition, train.matrix)
  Sale_Condition <- map.sale_condition[as.character(test$Sale_Condition)]
  test.matrix <- cbind(Sale_Condition, test.matrix)
  
  # remove x variables
  remove.var <- c('Utilities')

  # merge small levels
  categorical.vars <- setdiff(names(train), names(train.matrix))
  categorical.vars <- setdiff(categorical.vars, remove.var)
  p <- 1/10
  for (c in categorical.vars){
    small.factor <- names(which(prop.table(table(train[,c])) < p))
    levels(train[,c])[levels(train[,c]) %in% small.factor] <- "Other"
    small.factor <- names(which(prop.table(table(test[,c])) < p))
    levels(test[,c])[levels(test[,c]) %in% small.factor] <- "Other"
  }
  
  train.cate <- model.matrix(~., train[,categorical.vars])[,-1]
  test.cate <- model.matrix(~., test[,categorical.vars])[,-1]
  
  drop.trainindex <- setdiff(unlist(dimnames(train.cate)[2]), unlist(dimnames(test.cate)[2]))
  train.cate <- train.cate[,!colnames(train.cate) %in% drop.trainindex]
  drop.testindex <- setdiff(unlist(dimnames(test.cate)[2]), unlist(dimnames(train.cate)[2]))
  test.cate <- test.cate[,!colnames(test.cate) %in% drop.testindex]
  
  train.matrix <- cbind(train.cate, train.matrix)
  test.matrix <- cbind(test.cate, test.matrix)
  return(list(train = train.matrix, test = test.matrix))
}

