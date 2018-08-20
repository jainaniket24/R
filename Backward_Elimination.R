# Backward elimination method

# importing data
startUps = read.csv('50_Startups.csv')

# convert state variable into a dummy variable
startUps$State = factor(x = startUps$State)

# splitting data into test and train
library(caTools)
set.seed(123)
splitRatio = sample.split(startUps$Profit, SplitRatio = 0.80)
startUpTrain = startUps[splitRatio,]
startUpTest = startUps[!splitRatio,]

# fitting the model
startUpModel = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = startUpTrain)
summary(startUpModel)

# Max p-value is for state - removing state
startUpModel = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, data = startUpTrain)
summary(startUpModel)

# Max p-value is for Administration - removing Administration
startUpModel = lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data = startUpTrain)
summary(startUpModel)

# Max p-value is for Marketing Spend - removing Marketing Spend
startUpModel = lm(formula = Profit ~ R.D.Spend, data = startUpTrain)
summary(startUpModel)
