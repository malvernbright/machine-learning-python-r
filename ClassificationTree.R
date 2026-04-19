# Load Libraries
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(adabag)
library(xgboost)

df <- read.csv("Movie_classification.csv")

# Data Preprocessing
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken, na.rm = TRUE)

# Test-Train Split Fix
set.seed(0)
split <- sample.split(df$Start_Tech_Oscar, SplitRatio = 0.8) # Split on the target variable
trainc <- subset(df, split == TRUE)
testc <- subset(df, split == FALSE)

# --- Class Tree ---
classtree <- rpart(formula = Start_Tech_Oscar ~ ., data = trainc, 
                   method = 'class', control = rpart.control(maxdepth = 3))
rpart.plot(classtree, box.palette = "RdBu", digits = -3)
testc$pred <- predict(classtree, testc, type = "class")
table(testc$Start_Tech_Oscar, testc$pred)

# --- Bagging ---
set.seed(0)
# Note: Ensure 'Collection' is in your dataset for this regression task
bagging <- randomForest(Collection ~ ., data = trainc, mtry = 17, importance = TRUE)
testc$bagging_pred <- predict(bagging, testc)
MSE2bagging <- mean((testc$bagging_pred - testc$Collection)^2)

# --- Ada Boost ---
trainc$Start_Tech_Oscar1 <- as.factor(trainc$Start_Tech_Oscar)
adaboost <- boosting(Start_Tech_Oscar1 ~ . - Start_Tech_Oscar, data = trainc, boos = TRUE)
predada <- predict(adaboost, testc)
table(predada$class, testc$Start_Tech_Oscar)

# --- XGBOOST (Robust Fix) ---

# 1. Prepare Labels
trainY <- as.numeric(trainc$Start_Tech_Oscar == "1")
testY  <- as.numeric(testc$Start_Tech_Oscar == "1")

# 2. Create Model Matrices
trainX <- model.matrix(Start_Tech_Oscar ~ . - 1, data = trainc)
testX  <- model.matrix(Start_Tech_Oscar ~ . - 1, data = testc)

# 3. FIX: Aligning columns (Handling missing categories)
train_names <- colnames(trainX)
test_names  <- colnames(testX)

# Identify columns in Train that are missing in Test
missing_cols <- setdiff(train_names, test_names)

# Add missing columns to Test as 0s
if(length(missing_cols) > 0) {
  empty_matrix <- matrix(0, nrow = nrow(testX), ncol = length(missing_cols))
  colnames(empty_matrix) <- missing_cols
  testX <- cbind(testX, empty_matrix)
}

# Ensure Test has only the Train columns and in the exact same order
testX <- testX[, train_names, drop = FALSE]

# 4. Create DMatrix
Xmatrix   <- xgb.DMatrix(data = trainX, label = trainY)
Xmatrix_t <- xgb.DMatrix(data = testX, label = testY)

# 5. Parameters
params <- list(
  objective = "multi:softmax",
  num_class = 2,
  learning_rate = 0.3,
  max_depth = 6
)

# 6. Train & Predict
Xgboosting <- xgb.train(params = params, data = Xmatrix, nrounds = 50)
xgpred <- predict(Xgboosting, Xmatrix_t)

# Confusion Matrix
table(testY, xgpred)
