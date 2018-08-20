# Decision tree regression

# importing the dataset
salary <- read.csv('Position_Salaries.csv')
salary <- salary[, 2:3]

# fitting a decision tree regression model
library(rpart)

salary_tree <- rpart(formula = Salary ~ ., data = salary, 
                     control = rpart.control(minsplit = 1))
salary_pred <- predict(salary_tree, newdata = data.frame(Level = 6.5))

# visualizing the results
library(ggplot2)
x_grid <- seq(min(salary$Level), max(salary$Level), by = 0.01)
ggplot() +
  geom_point(aes(x = salary$Level, y = salary$Salary), col = 'red') +
  geom_line(aes(x = x_grid, y = predict(salary_tree, newdata = data.frame(Level = x_grid))), 
            col = 'blue') +
  ggtitle('Decision Tree Regression') + xlab('Levels') + ylab('Salary')
