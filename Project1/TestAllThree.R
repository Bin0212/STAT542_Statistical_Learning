# load data and train/test sample row index
data <- read.csv("Ames_data.csv")
load("project1_testIDs.Rdata")

for (i in 1:10){
  source('DataGenerate.R')
  DataGenerate(i)
  test.y <- read.csv("test_y.csv")
  
  source('mymain.R')
  
  pred1 <- read.csv("mysubmission1.txt")
  pred2 <- read.csv("mysubmission2.txt")
  pred3 <- read.csv("mysubmission3.txt")
  names(test.y)[2] <- "True_Sale_Price"
  pred1 <- merge(pred1, test.y, by="PID")
  pred2 <- merge(pred2, test.y, by="PID")
  pred3 <- merge(pred3, test.y, by="PID")
  #print(sqrt(mean((log(pred1$Sale_Price) - log(pred1$True_Sale_Price))^2)))
  #print(sqrt(mean((log(pred2$Sale_Price) - log(pred2$True_Sale_Price))^2)))
  print(sqrt(mean((log(pred3$Sale_Price) - log(pred3$True_Sale_Price))^2)))
}
