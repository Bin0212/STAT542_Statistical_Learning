DataGenerate = function(col.index){
  # generate the first training/test split
  j=col.index
  train <- data[-testIDs[,j], ]
  test <- data[testIDs[,j], ]
  test.y <- test[, c(1, 83)]
  test <- test[, -83]
  write.csv(train,"train.csv",row.names=FALSE)
  write.csv(test, "test.csv",row.names=FALSE)
  write.csv(test.y, "test_y.csv",row.names=FALSE)
}
