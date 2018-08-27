# Support Vector Machine Model

# importing the data
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

# Fitting the SVM model 
library(e1071)
ads_svm <- svm(formula = Purchased ~ ., data = ads_train, 
               type = 'C-classification', kernel = 'radial')

# predicting on test set
ads_pred <- predict(object = ads_svm, newdata = ads_test)

# creating the confusion matrix
ads_confMat <- table(ads_test$Purchased, ads_pred)
ads_confMat

# Visualizing the results on training set
library(ElemStatLearn)
data <- ads_train
x1 <- seq(min(data$Age) - 1, max(data$Age) + 1, by = 0.01)
x2 <- seq(min(data$EstimatedSalary) - 1, max(data$EstimatedSalary) + 1 , by = 0.01)
grid_data <- expand.grid(x1, x2)
colnames(grid_data) <- c('Age', 'EstimatedSalary')
data_prob <- predict(object = ads_svm, newdata = grid_data, type = 'response')
data_pred <- as.numeric(data_prob == 1)
plot(data[, -3], main = 'Kernel SVM  Training Set', 
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(data_pred, length(x1), length(x2)), add = TRUE)
points(grid_data, pch = '.', col = ifelse(data_pred == 1, 'springgreen3', 'tomato'))
points(data, pch = 21, bg = ifelse(data$Purchased == 1, 'green4', 'red3'))

# visualizing results: Test set
library(ElemStatLearn)
data <- ads_test
x1 <- seq(min(data$Age) - 1, max(data$Age) + 1, by = 0.01)
x2 <- seq(min(data$EstimatedSalary) - 1, max(data$EstimatedSalary) + 1 , by = 0.01)
grid_data <- expand.grid(x1, x2)
colnames(grid_data) <- c('Age', 'EstimatedSalary')
data_prob <- predict(object = ads_svm, newdata = grid_data, type = 'response')
data_pred <- as.numeric(data_prob == 1)
plot(data[, -3], main = 'Kernel SVM Test Set', 
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(data_pred, length(x1), length(x2)), add = TRUE)
points(grid_data, pch = '.', col = ifelse(data_pred == 1, 'springgreen3', 'tomato'))
points(data, pch = 21, bg = ifelse(data$Purchased == 1, 'green4', 'red3'))
