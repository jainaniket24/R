# K-Nearest Neighbors (KNN) Model

# importing the dataset
ads <- read.csv('Social_Network_Ads.csv')
summary(ads)
# removing id and gender columns from independent variables
ads <- ads[, 3:5]

# Scaling the data
ads[, 1:2] <- scale(ads[, 1:2])

# splitting the data into train and test set
library(caTools)
set.seed(2407)
train_index <- sample.split(ads$Purchased, SplitRatio = 0.75)
ads_train <- ads[train_index,]
ads_test <- ads[!train_index,]

# Fitting the KNN model and predicting the test set results
library(class)
ads_pred <- knn(train = ads_train[, 1:2], test = ads_test[, 1:2],
                cl = ads_train$Purchased, k = 5)

# Creating the confusion matrix
ads_confMat <- table(ads_test$Purchased, ads_pred)

# visualizing results: Training set
library(ElemStatLearn)
data <- ads_train
x1 <- seq(min(data$Age) - 1, max(data$Age) + 1, by = 0.01)
x2 <- seq(min(data$EstimatedSalary) - 1, max(data$EstimatedSalary) + 1 , by = 0.01)
grid_data <- expand.grid(x1, x2)
colnames(grid_data) <- c('Age', 'EstimatedSalary')
data_pred <- knn(train = ads_train[, 1:2], test = grid_data,
                 cl = ads_train$Purchased, k = 5)
plot(data[, -3], main = 'Logistic Regression Training Set', 
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(data_pred, length(x1), length(x2)), add = TRUE)
points(grid_data, pch = '.', col = ifelse(data_pred == 1, 'springgreen3', 'tomato'))
points(data, pch = 21, bg = ifelse(data$Purchased == 1, 'green4', 'red3'))

# visualizing results: Training set
data <- ads_test
x1 <- seq(min(data$Age) - 1, max(data$Age) + 1, by = 0.01)
x2 <- seq(min(data$EstimatedSalary) - 1, max(data$EstimatedSalary) + 1 , by = 0.01)
grid_data <- expand.grid(x1, x2)
colnames(grid_data) <- c('Age', 'EstimatedSalary')
data_pred <- knn(train = ads_train[, 1:2], test = grid_data,
                 cl = ads_train$Purchased, k = 5)
plot(data[, -3], main = 'Logistic Regression Training Set', 
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(data_pred, length(x1), length(x2)), add = TRUE)
points(grid_data, pch = '.', col = ifelse(data_pred == 1, 'springgreen3', 'tomato'))
points(data, pch = 21, bg = ifelse(data$Purchased == 1, 'green4', 'red3'))

