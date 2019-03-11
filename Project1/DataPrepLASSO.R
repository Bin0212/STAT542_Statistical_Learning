DataPrepLASSO = function(train, test){
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
  
  remove.vars <- c('Street', 'Utilities',  'Condition_2', 'Roof_Matl', 'Heating',
                  'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area', 
                  'Longitude','Latitude',"PID")
  
  train <- train[, !colnames(train) %in% remove.vars]
  test <- test[, !colnames(test) %in% remove.vars]
  
  winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", 
                   "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', 
                   "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", 
                   "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")
  
  for(var in winsor.vars){
    # apply winsorization on every numerical variables
    p <- 0.95
    # for train
    thresh <- quantile(train[ ,var], p)
    rp.index <- which(train[ ,var] > thresh)
    train[rp.index, var] <- thresh
    # for test
    thresh <- quantile(test[ ,var], p)
    rp.index <- which(test[ ,var] > thresh)
    test[rp.index, var] <- thresh
  }
  
  train.matrix <- model.matrix(~., train)[,-1]
  test.matrix <- model.matrix(~., test)[,-1]
  train.y <- log(train.matrix[,"Sale_Price"])
  
  drop.trainindex <- setdiff(unlist(dimnames(train.matrix)[2]), unlist(dimnames(test.matrix)[2]))
  train.matrix <- train.matrix[,!colnames(train.matrix) %in% drop.trainindex]
  drop.testindex <- setdiff(unlist(dimnames(test.matrix)[2]), unlist(dimnames(train.matrix)[2]))
  test.matrix <- test.matrix[,!colnames(test.matrix) %in% drop.testindex]
  
  return(list(train.y = train.y, train = train.matrix, test = test.matrix))
}