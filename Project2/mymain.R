# include all libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "text2vec",
  "xgboost",
  "glmnet",
  "pROC",
  "MASS",
  "doMC"
)

registerDoMC()

myvocab = scan(file = "myvocab.txt", what = character())

# read raw data into R
all <- read.table("Project2_data.tsv",stringsAsFactors = F,header = T)
# remove html tags (e.g. <div>, etc) in reviews
all$review <- gsub('<.*?>', ' ', all$review)

# read id for data splits
splits <- read.table("Project2_splits.csv", header = T)

# s <- 1 # Here we use the 3rd training/test split. 

# create training and test datasets
train <- all[-which(all$new_id%in%splits[,s]),]
test <- all[which(all$new_id%in%splits[,s]),]

# define function
prep_fun <- tolower
tok_fun <- word_tokenizer

# apply functions to traing and test datasets
it_train <- itoken(train$review,
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun)
it_test <- itoken(test$review,
                 preprocessor = prep_fun, 
                 tokenizer = tok_fun)

# define stop words for removal
stop_words <- c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "of", "one", "for", 
               "the", "us", "this")
# create vocabularies and its corresponding frequency
vocab <- create_vocabulary(it_train, ngram = c(1L,4L), stopwords = stop_words)
vocab = vocab[vocab$term %in% myvocab, ]

# filter out uncommon vocabularies
pruned_vocab <- prune_vocabulary(vocab,
                                term_count_min = 5, 
                                doc_proportion_max = 0.5,
                                doc_proportion_min = 0.001)

# define methods for creating document term matrix
bigram_vectorizer <- vocab_vectorizer(pruned_vocab)

# create document term matrix (dtm) 
# another matrix type term co-occurrence matrix (tcm)
dtm_train <- create_dtm(it_train, bigram_vectorizer)
dtm_test <- create_dtm(it_test, bigram_vectorizer)

################################ Word Selection ################################ 
# # perform two-sample t-test as screening method for word selection 
# # include package slam for sparse matrix calculation
# library(slam)
# v.size <- dim(dtm_train)[2]
# ytrain <- train$sentiment
# 
# summ <- matrix(0, nrow=v.size, ncol=4)
# summ[,1] <- colapply_simple_triplet_matrix(
#   as.simple_triplet_matrix(dtm_train[ytrain==1, ]), mean)
# summ[,2] <- colapply_simple_triplet_matrix(
#   as.simple_triplet_matrix(dtm_train[ytrain==1, ]), var)
# summ[,3] <- colapply_simple_triplet_matrix(
#   as.simple_triplet_matrix(dtm_train[ytrain==0, ]), mean)
# summ[,4] <- colapply_simple_triplet_matrix(
#   as.simple_triplet_matrix(dtm_train[ytrain==0, ]), var)
# 
# n1 <- sum(ytrain); 
# n <- length(ytrain)
# n0 <- n - n1
# 
# myp <- (summ[,1] - summ[,3]) / sqrt(summ[,2]/n1 + summ[,4]/n0)
# 
# # order the words by the magnitude of their t-statistics, pick the top 2000 words, 
# # which are then divide into two lists: positive words and negative words. 
# 
# words = colnames(dtm_train)
# id = order(abs(myp), decreasing=TRUE)[1:2000]
# pos.list = words[id[myp[id]>0]]
# neg.list = words[id[myp[id]<0]]
# 
# pos.list[1:50]
# neg.list[1:50]
# 
# write(words[id], file="myvocab.txt")

######################### Logistic Regression w/ LASSO ######################### 
# perform cross validation on LASSO
cv.out <- cv.glmnet(dtm_train, train[,2], family="binomial", alpha = 1, parallel=TRUE)
# perform prediction on LASSO with lambda.min
# coef <- predict(cv.out, s = cv.out$lambda.min, newx = dtm_test, type="coefficients")
# make predictions for test dataset
predict.y <-predict(cv.out, s = cv.out$lambda.min, newx = dtm_test)

# write to mysubmission1
write.table(cbind(test[,1] ,predict.y), file = "mysubmission1.txt", 
            sep = ", ", row.names = FALSE,col.names = c('new_id', 'prob'), 
            quote = FALSE)

############################ Discriminant Analysis ############################# 
# fit <- lda(dtm_train, train[,2])
# predict.y <- predict(fit, dtm_test)
# auc(test[,2], as.numeric(predict.y$class))

# try PCA + FDA
Sigma = cov(as.matrix(dtm_train))
dig.eigen=eigen(Sigma);
PCA.dir = dig.eigen$ve[, 1:1500]  # pick top 1000 PCA directions

newX = as.matrix(dtm_train) %*% PCA.dir[, 1:1500];
newdig.lda = lda(newX, train[,2]);
newXtest = as.matrix(dtm_test) %*% PCA.dir[, 1:1500]
predict.y = predict(newdig.lda, newXtest)$class

# write to mysubmission2
write.table(cbind(test[,1] ,predict.y), file = "mysubmission2.txt", 
            sep = ", ", row.names = FALSE,col.names = c('new_id', 'prob'), 
            quote = FALSE)

################# Logistic Regression w/ Boosting Tree + LASSO################## 
train.y = train$sentiment
test.y = test$sentiment

param = list(max_depth = 2, 
             subsample = 0.5, 
             objective='binary:logistic')

ntrees = 500
set.seed(8259)
bst = xgb.train(params = param, 
                data = xgb.DMatrix(data = dtm_train, label = train.y),
                nrounds = ntrees, 
                nthread = 2)

dt = xgb.model.dt.tree(model = bst)
words = unique(dt$Feature[dt$Feature != "Leaf"])
# words
# length(words) # use 1019 words

new_feature_train = xgb.create.features(model = bst, dtm_train)
new_feature_train = new_feature_train[, - c(1:ncol(dtm_train))]
new_feature_test = xgb.create.features(model = bst, dtm_test)
new_feature_test = new_feature_test[, - c(1:ncol(dtm_test))]
c(ncol(new_feature_test), ncol(new_feature_train))

cv.out <- cv.glmnet(new_feature_train, train[,2], family="binomial", alpha = 1, parallel=TRUE)
# perform prediction on LASSO with lambda.min
# coef <- predict(cv.out, s = cv.out$lambda.min, newx = new_feature_test, type="coefficients")
# make predictions for test dataset
predict.y <-predict(cv.out, s = cv.out$lambda.min, newx = new_feature_test)

# write to mysubmission3
write.table(cbind(test[,1] ,predict.y), file = "mysubmission3.txt", 
            sep = ", ", row.names = FALSE,col.names = c('new_id', 'prob'), 
            quote = FALSE)
