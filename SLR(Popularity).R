rm(list = ls()) # Removing All the Existing Variables

# Importing the dataset
dataset <- read.csv("D:/SEM_VI(VAC)/New folder/Datasets/tmdb_5000_movies.csv")
dataset=dataset[c('budget','popularity','runtime','vote_average','vote_count','revenue')]
View(dataset)

# Slicing For SLR
pop = dataset[,2]
rev = dataset[,6]

# Framing The Sliced datas
data = data.frame(pop,rev)
View(data)

# Importing Libraries
library(caTools)

# Generate Random seed
set.seed(42)

# Splitting
split = sample.split(data$rev, SplitRatio = 0.8)

# Training Set
train_set = subset(data, split = TRUE)

# Testing Set
test_set = subset(data, split = FALSE)

# Applying Regression
regressor = lm(formula = rev~pop, data = train_set)

# Summary
summary(regressor)

# Prediction
y_pred = predict(regressor, newdata = test_set)

# Compare
comp = data.frame(test_set$rev, y_pred)
View(comp)

# Visualization
plot(test_set$pop, test_set$rev, type='p', col='blue',
     xlab = "Popularity", ylab = "Revenue")
lines(test_set$rev, y_pred, type = 'o', col='red')

# Importing Library for Performance Metrics
library(caTools)

# Mean Absolute Error
MAE(y_pred, test_set$rev)

# Root Mean Square Error
RMSE(y_pred, test_set$rev)

# R-2 Square error
R2(y_pred, test_set$rev)
