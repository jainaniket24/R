#### Natural Language Procesing - Bag of Words model ####

# Importing the dataset
# reviews <- read.csv('Restaurant_Reviews.tsv', sep = '\t', quote = '', 
#                    stringsAsFactors = FALSE)

# a better way to import tab delimitted file is using read.delim func:
reviews <- read.delim('Restaurant_Reviews.tsv', quote = '', 
                      stringsAsFactors = FALSE)

#### cleaning the texts ####
#install.packages('tm')
#install.packages('SnowballC')
library(tm)
library(SnowballC)
reviews_corpus <- VCorpus(VectorSource(x = reviews$Review))
# converting reviews to lower case
reviews_corpus <- tm_map(x = reviews_corpus, FUN = content_transformer(tolower))
# removing the numbers
reviews_corpus <- tm_map(x = reviews_corpus, removeNumbers)
# removing punctuations
reviews_corpus <- tm_map(x = reviews_corpus, removePunctuation)
# removing irrelevant words (like prepositions, conjuctions, articles)
reviews_corpus <- tm_map(x = reviews_corpus, removeWords, stopwords())
# converting the words to root (stemming)
reviews_corpus <- tm_map(x = reviews_corpus, stemDocument)
# removing extra spaces
reviews_corpus <- tm_map(x = reviews_corpus, stripWhitespace)

#### creating the bag of words model ####
# creating sparse matrix
reviews_dtm <- DocumentTermMatrix(x = reviews_corpus)
# removing the words that appear only once
reviews_dtm <- removeSparseTerms(x = reviews_dtm, sparse = 0.999)

#### Making the classificaton model ####
# using the Random Forest Model
library(randomForest)
# converting the dtm to data frame
review_df <- as.data.frame(as.matrix(reviews_dtm))
# adding the 'liked' column to the data frame
review_df$liked <- as.factor(reviews$Liked)
# splitting into train and test set
library(caTools)
train_index <- sample.split(Y = review_df$liked, SplitRatio = 0.80)
reviews_train <- review_df[train_index, ]
reviews_test <- review_df[!train_index, ]

# fitting the model
reviews_rf <- randomForest(x = reviews_train[, -ncol(reviews_train)],
                           y = reviews_train$liked, ntree = 100)

# predicting the values on test set
reviews_pred <- predict(object = reviews_rf, newdata = reviews_test, type = 'response')

# making the confusion matrix
reviews_confMat <- table(reviews_test$liked, reviews_pred)
reviews_confMat

reviews_accuracy <- sum(diag(reviews_confMat)) / nrow(reviews_test)
reviews_accuracy
# considering we only had 800 reviews to train our model on, 75% is a decent
# accuracy for a text analytics model!
