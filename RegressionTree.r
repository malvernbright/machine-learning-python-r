movie <- read.csv("Movie_regression.csv")
View(movie)

# Data Pre-processing
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken, na.rm = TRUE)
summary(movie)


## Test-Train Split
install.packages("caTools")
library(caTools)
set.seed(0)
split <- sample.split(movie, SplitRatio = .8)
train <- subset(movie, split = TRUE)
test <- subset(movie, split = FALSE)

## Regression Tree
install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

## Run regression tree model on train set
regtree <- rpart(formula = Collection~., data = train, control = rpart.control(maxdepth = 3))
# press F1 on rpart for help on this function

# Plot the decision tree
rpart.plot(regtree, box.palette = "RdBu", digits = -3)

# Predict valuebat any point
test$pred <- predict(regtree, test, type = "vector")

MSE2 <- mean((test$pred - test$Collection)^2)
