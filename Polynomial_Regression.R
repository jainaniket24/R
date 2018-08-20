##### Polynomial regression #####

# importing data
salary <- read.csv('Position_Salaries.csv')

# column level (column 2) is a dummy coded version of position column (column 1)
# therefore removing column 1
salary <- salary[, -1]

# fitting a linear model
salary_lm <- lm(formula = Salary ~ ., data = salary)
summary(salary_lm)

# fitting a polynomial regression model
# creating new columns with square and cube of values in level column
salary$level2 <- salary$Level^2
salary$level3 <- salary$Level^3
salary_poly <- lm(formula = Salary ~ ., data = salary)
summary(salary_poly)

# visualizing linear model
library(ggplot2)
ggplot() + 
  geom_point(aes(x = salary$Level, y = salary$Salary), col = 'red') +
  geom_line(aes(x = salary$Level, y = predict(salary_lm, newdata = salary)), col = 'blue') +
  ggtitle('Linear Regression Results') +
  xlab('Level') +
  ylab('Salary')


# visualizing polynomial model
ggplot() + 
  geom_point(aes(x = salary$Level, y = salary$Salary), col = 'red') +
  geom_line(aes(x = salary$Level, y = predict(salary_poly, newdata = salary)), col = 'blue') +
  ggtitle('Linear Regression Results') +
  xlab('Level') +
  ylab('Salary')

