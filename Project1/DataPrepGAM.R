DataPrepGAM = function(train, test){
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
                  'Longitude','Latitude', 'Mo_Sold', 'Year_Sold',
                  'PID')
  
  train <- train[, !colnames(train) %in% remove.vars]
  test <- test[, !colnames(test) %in% remove.vars]
  
  linear.vars <- c('BsmtFin_SF_1', 'Bsmt_Full_Bath', 'Bsmt_Half_Bath', 
                   'Full_Bath', 'Half_Bath', 'Bedroom_AbvGr', 
                   'Kitchen_AbvGr', 'Fireplaces', 'Garage_Cars')
  
  categorical.vars <- colnames(train)[which(sapply(train, 
                                                       function(x) is.factor(x)))]
  num.vars <- names(train)
  num.vars <- num.vars[num.vars != "Sale_Price"]
  num.vars <- num.vars[! num.vars %in% categorical.vars]
  num.vars <- num.vars[! num.vars %in% linear.vars]
  
  # select level variable based on LASSO.1se
  select.level.var = c('MS_SubClass__One_Story_1945_and_Older',
                       'MS_SubClass__Two_Story_PUD_1946_and_Newer',
                       'MS_Zoning__Residential_Low_Density',
                       'MS_Zoning__Residential_Medium_Density', 'Lot_Area',
                       'Neighborhood__Crawford', 'Neighborhood__Edwards',
                       'Neighborhood__Green_Hills', 'Neighborhood__Iowa_DOT_and_Rail_Road',
                       'Neighborhood__Meadow_Village','Neighborhood__Northridge',
                       'Neighborhood__Somerset', 'Neighborhood__Stone_Brook',
                       'Condition_1__Norm', 'Condition_1__RRAe',
                       'Bldg_Type__OneFam', 'Bldg_Type__Twnhs',
                       'Overall_Qual__Average', 'Overall_Qual__Below_Average',
                       'Overall_Qual__Excellent', 'Overall_Qual__Fair',
                       'Overall_Qual__Good', 'Overall_Qual__Poor',
                       'Overall_Qual__Very_Excellent', 'Overall_Qual__Very_Good',
                       'Overall_Cond__Average', 'Overall_Cond__Below_Average',
                       'Overall_Cond__Excellent', 'Overall_Cond__Fair',
                       'Overall_Cond__Good', 'Overall_Cond__Poor',
                       'Overall_Cond__Very_Good', 'Overall_Cond__Very_Poor',
                       'Exterior_1st__BrkFace', 'Mas_Vnr_Area',
                       'Exter_Qual__Typical', 'Exter_Cond__Fair',
                       'Foundation__PConc', 'Bsmt_Exposure__Gd',
                       'Bsmt_Exposure__No', 'BsmtFin_Type_1__GLQ',
                       'BsmtFin_SF_1', 'Bsmt_Unf_SF', 'First_Flr_SF',
                       'Gr_Liv_Area', 'Bsmt_Full_Bath', 'Full_Bath',
                       'Heating_QC__Fair', 'Heating_QC__Typical',
                       'Kitchen_Qual__Fair', 'Kitchen_Qual__Typical',
                       'Functional__Sal', 'Functional__Sev', 'Functional__Typ',
                       'Fireplaces', 'Fireplace_Qu__Good', 'Garage_Cars',
                       'Garage_Area', 'Garage_Qual__Good', 'Garage_Cond__Fair',
                       'Garage_Cond__Typical', 'Paved_Drive__Paved',
                       'Wood_Deck_SF', 'Open_Porch_SF', 'Screen_Porch',
                       'Sale_Type__New', 'Sale_Condition__Partial'
                       )


  m <- length(select.level.var)
  n.train <- nrow(train)
  n.test <- nrow(test)
  tmp.train <- matrix(0, n.train, m)
  tmp.test <- matrix(0, n.test, m)
  colnames(tmp.train) <- select.level.var
  colnames(tmp.test) <- select.level.var
  for(i in 1:m){
    tmp <- unlist(strsplit(select.level.var[i], '__'))
    select.var <- tmp[1]
    select.level <- tmp[2]
    tmp.train[train[, select.var]==select.level, i] <- 1
    tmp.test[test[, select.var]==select.level, i] <- 1
  }
  
  # construct train/test matrix
  train.GAM <- cbind(Sale_Price = log(train[,'Sale_Price']), train[,linear.vars], train[,num.vars], tmp.train)
  test.GAM <- cbind(test[,linear.vars], test[,num.vars], tmp.test)
  
  # return
  return(list(select.level.var = select.level.var, linear.vars = linear.vars, 
              num.vars = num.vars, train = train.GAM, test = test.GAM))
}