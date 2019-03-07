# Get Journal of Statisticsl Software (JSS) abstract data
install.packages("corpus.JSS.papers", repos = "http://datacube.wu.ac.at/", type = "source")
data("JSS_papers", package = "corpus.JSS.papers")

# Remove those containing non-ASCII characters in the abstracts.
JSS_papers = JSS_papers[sapply(JSS_papers[, "description"],Encoding) == "unknown",]
dim(JSS_papers)

JSS_papers[1,]
range(JSS_papers[, "date"])

# Install other relevant packages.
install.packages("tm")
install.packages("SnowballC")
install.packages("topicmodels")
install.packages("wordcloud")
install.packages("RColorBrewer")

# Use the tm package to transform the data to a Corpus and then to a 
# DocumentTermMatrix.
# When transforming data to a DocumentTermMatrix, we apply the following steps: 
# terms are stemmed and the stop words, punctuation, numbers and terms of length 
# less than 3 are removed (in the control argument).
library(tm)
library(SnowballC) # needed for stemdocument
myCorpus = Corpus(VectorSource(JSS_papers[, "description"]))

JSS_dtm = DocumentTermMatrix(myCorpus, 
                             control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3, 
                                            removeNumbers = TRUE, removePunctuation = TRUE))

dim(JSS_dtm)

words=colnames(JSS_dtm)
words[1:20]

JSS_dtm[,1]

# Use tf-idf (term frequency-inverse document frequency) to select the vocabulary.
# Goal: remove terms with low frequences as well as those occurring in many 
# documents. We only include terms whose tf-idf values are at least 0.1, which 
# is a bit more than the median and ensures that the very frequent terms are 
# omitted.
library("slam")
summary(col_sums(JSS_dtm))
term_tfidf = tapply(JSS_dtm$v/row_sums(JSS_dtm)[JSS_dtm$i], JSS_dtm$j, mean)*
  log2(nDocs(JSS_dtm)/col_sums(JSS_dtm > 0))

summary(term_tfidf)

JSS_dtm =  JSS_dtm[, term_tfidf >= 0.1]
JSS_dtm = JSS_dtm[row_sums(JSS_dtm) > 0,]
summary(col_sums(JSS_dtm))

dim(JSS_dtm)
words=colnames(JSS_dtm)
words[1:20]

# Fit an LDA model with 30 topics using VEM (Variational EM)
# The output is an LDA object, from which we obtain

# a mixing weight vector (over 30 topics) for each document,
# a term frequency vector (over all words) for each of the 30 topic,
# and other summaries from this topic model.
library("topicmodels")
k = 30
VEM = LDA(JSS_dtm, k = k,method = "VEM")
# a mixing weight vector (over 30 topics) for each document
dim(posterior(VEM)$topics)
# a term frequency vector (over all words) for each of the 30 topic
dim(posterior(VEM)$terms)
# 5 most frequent terms for each top
freq.Terms = terms(VEM, 5)
dim(freq.Terms)
freq.Terms

# Generate word clouds
library(wordcloud)
library(RColorBrewer)

topic_terms = posterior(VEM)$terms

# Wordcloud for all topics
v = sort(colSums(topic_terms), decreasing=TRUE);
myNames = names(v);
d = data.frame(word=myNames, freq=v);
wordcloud(d$word, d$freq, max.words=50, scale=c(4,.2), random.order=FALSE, rot.per=.25, 
          colors=brewer.pal(8,"Dark2"))

# Wordcloud for individual topics
topic.id = c(4, 6, 8, 12, 13, 27)
par(mfrow=c(2,3))
for(i in 1:6){
  v = sort(topic_terms[topic.id[i],], decreasing=TRUE);
  myNames = names(v);
  d = data.frame(word=myNames, freq=v);
  wordcloud(d$word, d$freq, max.words=30, scale=c(4,.3), random.order=FALSE, rot.per=.25, 
            colors=brewer.pal(8,"Dark2"))
}






