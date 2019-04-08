library(ElemStatLearn)
library(MASS)

data("zip.train")
data("zip.test")

dig.lda = lda(zip.train[,2:257],zip.train[,1]);
Ytest.pred = predict(dig.lda, zip.test[,2:257])$class
pred = predict(dig.lda, zip.test[,2:257])
table(zip.test[,1], Ytest.pred)
