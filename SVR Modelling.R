# Support Vector Regression
# loading data
salary <- read.csv('Position_Salaries.csv')
salary <- salary[, 2:3]

# fitting the SVR model on the data
install.packages('e1071')
library(e1071)
salary_svr <- svm(formula = Salary ~ ., data = salary, type = 'eps-regression')
y_pred <- predict(salary_svr, newdata = data.frame(Level = 6.5))

# visualization of results
library(ggplot2)
ggplot() +
  geom_point(aes(x = salary$Level, y = salary$Salary), col = 'red') +
  geom_line(aes(x = salary$Level, y = predict(salary_svr, newdata = salary)), 
            col = 'blue') +
  ggtitle('SVR Model') + xlab('Levels') + ylab('Salary')
  

