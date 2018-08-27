# Decision tree classification

# importing the data
ads <- read.csv('Social_Network_Ads.csv')
ads <- ads[, 3:5]

# splitting into train and test set
library(caTools)
set.seed(123)
train_index <- sample.split(ads$Purchased, SplitRatio = 0.75)
ads_train <- ads[train_index, ]
ads_test <- ads[!train_index, ]

# fitting the Decision Tree
library(rpart)
ads_cart <- rpart(formula = Purchased ~ ., data = ads_train)

# Predicting the results
ads_pred <- predict(object = ads_cart, newdata = ads_test)
ads_pred <- as.numeric(ads_pred > 0.5)

# confusion matrix
ads_confMat <- table(ads_test$Purchased, ads_pred)

# plotting the tree
plot(ads_cart)
text(ads_cart)

