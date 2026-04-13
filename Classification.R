df <- read.csv("Classification_preprocessed_data_R.csv", header = TRUE )
# Logistic Regression with a single predictor

glm.fit <- glm(Sold~price, data = df, family = binomial)
summary(glm.fit)

#Logistic Regression with Multiple predictor
glm.fit = glm(Sold~., data = df, family = binomial)
summary(glm.fit)

glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]

glm.pred = rep("NO", 506)
glm.pred[glm.probs > .5] <- "YES"

table(glm.pred,df$Sold)

# LDA
install.packages("MASS")
require("MASS")
lda.fit <- lda(Sold~., data = df)
lda.fit

lda.pred <- predict(lda.fit, df)

lda.pred$posterior

lda.class <- lda.pred$class
# confusion matrix
table(lda.class, df$Sold)

sum(lda.pred$posterior[ ,1] > .8)


## Test Train Split
install.packages("caTools")
require("caTools")

set.seed(0)
split <- sample.split(df, SplitRatio = .8)
train_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

train.fit <- glm(Sold~., data = train_set, family = binomial)
test.probs <- predict(train.fit, test_set, type = "response")
test.pred <- rep('NO',120)
test.pred[test.probs > .5] = "YES"
table(test.pred,test_set$Sold)
