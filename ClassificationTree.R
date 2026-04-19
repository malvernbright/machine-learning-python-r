df <- read.csv("Movie_classification.csv")
View(df)

# Data Preprocessing
summary(df)
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken, na.rm = TRUE)
summary(df)

# Test-Train Split
install.packages("caTools")
library(caTools)
set.seed(0)
split <- sample.split(df, SplitRatio = .8)
trainc <- subset(df, split = TRUE)
testc <- subset(df, split = FALSE)

## Class Tree
install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

## Run regression tree model on train set
classtree <- rpart(formula = Start_Tech_Oscar~., data = trainc, method = 'class', control = rpart.control(maxdepth = 3))
# press F1 on rpart for help on this function

# Plot the decision tree
rpart.plot(classtree, box.palette = "RdBu", digits = -3)

# Predict value at any point
testc$pred <- predict(classtree, testc, type = "class")

table(testc$Start_Tech_Oscar, testc$pred)

# Bagging
install.packages("randomForest")
library(randomForest)
set.seed(0)
bagging = randomForest(Collection~., data = trainc, mtry=17)
testc$bagging <- predict(bagging, testc)
MSE2bagging <- mean((testc$bagging - testc$Collection)^2)
bagging = randomForest(Collection~., data = trainc, mtry = 7, importance = TRUE)
which(is.na(bagging))


# Ada Boost
install.packages("adabag")
library(adabag)


trainc$Start_Tech_Oscar1 <- as.factor(trainc$Start_Tech_Oscar)

adaboost <- boosting(Start_Tech_Oscar1~.-Start_Tech_Oscar, data = trainc, boos=TRUE)

predada <- predict(adaboost, testc)
table(predada$class, testc$Start_Tech_Oscar)

t1 <- adaboost$trees[[1]]
plot(t1)
text(t1, pretty = 100)
