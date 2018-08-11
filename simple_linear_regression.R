# Simple Linear Regression

# Importing the dataset
salary = read.csv('Salary_Data.csv')

# data preprocessing

## splitting data into test and train set
library('caTools')
set.seed(123)
splitRatio <- sample.split(salary$Salary, SplitRatio = 2/3)
salaryTrain <- salary[splitRatio,]
salaryTest <- salary[!splitRatio,]

# fitting linear regression model
salarylm <- lm(formula = Salary ~ YearsExperience, data = salaryTrain)

# predicting the test set results
salaryPredict <- predict(object = salarylm, newdata = salartTest)

# visualizing the results
library(ggplot2)
# training set results
ggplot() + 
  geom_point(aes(x = salaryTrain$YearsExperience, y = salaryTrain$Salary), 
             color = 'red') +
  geom_line(aes(x = salaryTrain$YearsExperience, y = predict(salarylm, newdata = salaryTrain)),
            color = 'blue') +
  ggtitle("Salary vs YearsExperience (Training set)") +
  xlab('Years of Experience') +
  ylab('Salary')

# test set results
ggplot() + 
  geom_point(aes(x = salartTest$YearsExperience, y = salartTest$Salary), 
             color = 'red') +
  geom_line(aes(x = salaryTrain$YearsExperience, y = predict(salarylm, newdata = salaryTrain)),
            color = 'blue') +
  ggtitle("Salary vs YearsExperience (Training set)") +
  xlab('Years of Experience') +
  ylab('Salary') 
