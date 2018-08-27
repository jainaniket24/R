# Random Forest Classification

# importing the data
ads <- read.csv('Social_Network_ads.csv')
ads <- ads[, 3:5]

# splitting data into train and test data
library(caTools)
set.seed(123)
train_index <- sample.split(Y = ads$Purchased, SplitRatio = 0.75)
ads_train <- ads[train_index, ]
ads_test <- ads[!train_index, ]

# building the Random forest model
library(randomForest)
ads_rf <- randomForest(x = ads_train[,1:2], y = ads_train[, 3], ntree = 10)

# making prediction
ads_pred <- predict(object = ads_rf, newdata = ads_test[-3])

# making the confusion matrix
ads_confMat <- table(ads_test$Purchased, ads_pred > 0.5)
