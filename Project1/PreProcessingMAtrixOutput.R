PreProcessingMatrixOutput <- function(train.data, test.data){
  # generate numerical matrix of the train/test
  # assume train.data, test.data have the same columns
  categorical.vars <- colnames(train.data)[which(sapply(train.data, 
                                                        function(x) is.factor(x)))]
  train.matrix <- train.data[, !colnames(train.data) %in% categorical.vars, drop=FALSE]
  test.matrix <- test.data[, !colnames(train.data) %in% categorical.vars, drop=FALSE]
  n.train <- nrow(train.data)
  n.test <- nrow(test.data)
  for(var in categorical.vars){
    mylevels <- sort(unique(train.data[, var]))
    m <- length(mylevels)
    tmp.train <- matrix(0, n.train, m)
    tmp.test <- matrix(0, n.test, m)
    col.names <- NULL
    for(j in 1:m){
      tmp.train[train.data[, var]==mylevels[j], j] <- 1
      tmp.test[test.data[, var]==mylevels[j], j] <- 1
      col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
    }
    colnames(tmp.train) <- col.names
    colnames(tmp.test) <- col.names
    train.matrix <- cbind(train.matrix, tmp.train)
    test.matrix <- cbind(test.matrix, tmp.test)
  }
  return(list(train = train.matrix, test = test.matrix))
}