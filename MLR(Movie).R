rm(list = ls()) # Removing All the Existing Variables

# Importing the dataset
dataset <- read.csv("D:/SEM_VI(VAC)/New folder/Datasets/tmdb_5000_movies.csv")
dataset=dataset[c('budget','popularity','runtime','vote_average','vote_count','revenue')]
View(dataset)

# Importng Libraries
library(caTools)

#Setting Seed
set.seed(123)

#Splitting Data
split = sample.split(dataset$revenue, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Regression Formula
regressor = lm(formula = revenue ~ .,
               data = training_set)

# Summary
summary(regressor)

# Prediction
y_pred = predict(regressor, newdata = test_set)

# Compare
comp = data.frame(test_set$revenue, y_pred)
View(comp)

# Visualization
plot(test_set$revenue,type = 'o', col = 'blue', xlab = 'Test data'
     ,ylab = 'Revenue')
lines(y_pred,type='o',col='red')

# Importing Library for Performance Metrics
library(caret)

# Mean Absolute Error
MAE(y_pred, test_set$revenue)

# Root Mean Square Error
RMSE(y_pred, test_set$revenue)

# R-square Score
R2(y_pred, test_set$revenue)
