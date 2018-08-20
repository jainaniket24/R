# Random Forest Regression Model

# importing the dataset
salary <- read.csv('Position_Salaries.csv')
salary <- salary[, 2:3]

# building and fitting the random forest model
library(randomForest)
set.seed(1234)
salary_rf <- randomForest(x = salary[1], y = salary$Salary, ntree = 500)

# making predictions
salary_pred <- predict(salary_rf, newdata = data.frame(Level = 6.5))

# visualization results
library(ggplot2)
x_grid <- seq(min(salary$Level), max(salary$Level), by = 0.01)
ggplot() +
  geom_point(aes(salary$Level, salary$Salary), col = 'red') +
  geom_line(aes(x_grid, predict(salary_rf, newdata = data.frame(Level = x_grid))), col = 'blue') +
  ggtitle('Random Forest Model') + xlab('Level') + ylab('Salary')
  