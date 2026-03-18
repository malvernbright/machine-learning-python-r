df <- read.csv("House_Price.csv", header = TRUE )
str(df)

summary(df)

hist(df$crime_rate) # Histogram
pairs(~price+crime_rate+n_hot_rooms+rainfall, data = df) # Scatter plot
barplot(table(df$airport))
barplot(table(df$bus_ter))
barplot(table(df$waterbody))

# Observations
# n_hot_rooms and rainfall has outliers
# n_hos_beds has missing values
# bus_ter is a useless variable
# crime_rate has some other functional relationships with price

# Flooring and capping - value imputation
quantile(df$n_hot_rooms, .99)
quantile(df$room_num, .99)
uv = 3*quantile(df$n_hot_rooms, .99)
df$n_hot_rooms[df$n_hot_rooms>uv] <- uv
summary(df$n_hot_rooms)

lv = .3*quantile(df$rainfall, .01)
df$rainfall[df$rainfall<lv] <- lv
summary(df$rainfall)

# Missing Value
# room_num
# n_hos_beds
# waterbody
mean(df$n_hos_beds)
mean(df$n_hos_beds, na.rm = TRUE)
mean(df$room_num, na.rm = TRUE)
which(is.na(df$n_hos_beds))
which(is.na(df$room_num))
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds, na.rm = TRUE)
df$room_num[is.na(df$room_num)] <- mean(df$room_num, na.rm = TRUE)
summary(df$n_hos_beds)
summary(df$room_num)
mean(df$n_hos_beds)
which(is.na(df$n_hos_beds))
which(is.na(df$room_num))
pairs(~price+crime_rate, data = df)

plot(df$price, df$crime_rate)

df$crime_rate <- log(1 + df$crime_rate)
df$room_num <- log(1 + df$room_num)
plot(df$price, df$crime_rate)
plot(df$price, df$room_num)
df$avg_dist <- (df$dist1 + df$dist2 + df$dist3 + df$dist4)/4

df2 <- df[, -7:-10]
df <- df2
rm(df2)
rm(df3)
df <- df[,-14]

#df <- dummy_cols(df, select_columns = c("airport", "waterbody"))
# install fastDummies
# install.packages("fastDummies")
require("fastDummies")
df <- dummy_cols(df, select_columns = c("airport", "waterbody"), remove_selected_columns = TRUE)
df$waterbody_lake_and_river <- df$`waterbody_Lake and River`
df$`waterbody_Lake and River` <- NULL
summary(df)
cor(df)
round(cor(df), 2)
df <- df[,-12] # Remove Parks
df <- df[, -13]
df <- df[, -15]

# Linear Regression
simple_model <- lm(price~room_num, data = df)
summary(simple_model)

plot(df$room_num, df$price)
abline(simple_model)

# Multiple Linear Regression
multiple_model <- lm(price~., data=df)
summary(multiple_model)

# Test Train Split
install.packages("caTools")
require("caTools")
set.seed(0)
split <- sample.split(df, SplitRatio = .8)
training_set <- subset(df, split==TRUE)
test_set <- subset(df, split==FALSE)

lm_a <- lm(price~.,data=training_set)
summary(lm_a)
train_a <- predict(lm_a, training_set)
test_a <- predict(lm_a, test_set)

mean((training_set$price - train_a)^2)
mean((test_set$price - test_a)^2)

# Subset Selection
install.packages("leaps")
require("leaps")

lm_best <- regsubsets(price~.,data = df, nvmax = 15)
summary(lm_best)
summary(lm_best)$adjr2
which.max(summary(lm_best)$adjr2)

coef(lm_best, 8)

lm_forward <- regsubsets(price~.,data = df, nvmax = 15, method = "forward")
summary(lm_forward)
lm_backward <- regsubsets(price~.,data = df, nvmax = 15, method = "backward")
summary(lm_backward)

# Ridge
install.packages("glmnet")
require("glmnet")
x <- model.matrix(price~.,data = df)[,-1]
y <- df$price
grid <- 10^seq(10,-2,length=100)
grid

lm_ridge <- glmnet(x,y,alpha = 0, lambda = grid)
summary(lm_ridge)
cv_fit <- cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv_fit)

opt_lambda <- cv_fit$lambda.min
tss <- sum((y-mean(y))^2)

y_a <- predict(lm_ridge,s=opt_lambda, newx = x)

rss <- sum((y_a-y)^2)

rsq <- 1 - rss/tss

lm_lasso <- glmnet(x,y,alpha = 0, lambda = grid)
y_lasso <- predict(lm_lasso,s=opt_lambda, newx = x)
rss2 <- sum((y_lasso-y)^2)
rsq2 <- 1 - rss2/tss
