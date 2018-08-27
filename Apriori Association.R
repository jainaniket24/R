# Association rule using Apriori Algorithm

# importing the data
mkt_bskt <- read.csv('Market_Basket_Optimisation.csv', header = FALSE)

# building the sparse matrix
# install.packages('arules')
library(arules)
mkt_bskt_sparse <- read.transactions('Market_Basket_Optimisation.csv', sep = ',',
                                     rm.duplicates = TRUE)
summary(mkt_bskt_sparse)
itemFrequencyPlot(x = mkt_bskt_sparse, topN = 20)

# Training Apriori on the dataset
mkt_bskt_rules <- apriori(data = mkt_bskt_sparse,
                          parameter = list(support = 0.004, confidence = 0.5))
# confidence - product must be purchased at least 3 times a week
# 3*7 / 7500

# visualizing the rules
inspect(sort(mkt_bskt_rules, by = 'lift')[1:10])


