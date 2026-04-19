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

# Predict value at any point
test$pred <- predict(regtree, test, type = "vector")

MSE2 <- mean((test$pred - test$Collection)^2)


# Trees
fulltree <- rpart(formula = Collection~., data = train, control = rpart.control(cp=0))
rpart.plot(fulltree, box.palette = "RdBu", digits = -3)
printcp(fulltree)
plotcp(regtree)


mincp <- regtree$cptable[which.min(regtree$cptable[,"xerror"]), "CP"]

prunedtree <- prune(regtree, cp=mincp)
rpart.plot(prunedtree, box.palette = "RdBu", digits = -3)

test$fulltree <- predict(fulltree, test, type = "vector")
MSE2 <- mean((test$fulltree - test$Collection)^2)

test$pruned <- predict(prunedtree, test, type = "vector")
MSE2pruned <- mean((test$pruned - test$Collection)^2)


# Bagging
install.packages("randomForest")
library(randomForest)
set.seed(0)
bagging = randomForest(Collection~., data = train, mtry=17)
test$bagging <- predict(bagging, testc)
MSE2bagging <- mean((test$bagging - test$Collection)^2)
bagging = randomForest(Collection~., data = train, mtry = 7, importance = TRUE)
which(is.na(bagging))


# Random Forest
randomFor <- randomForest(Collection~., data = train, ntree = 500)

# Predict output
test$random <- predict(randomFor, test)
MSE2random <- mean((test$random - test$Collection)^2)

