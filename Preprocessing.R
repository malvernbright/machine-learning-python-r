df <- read.csv("/Users/malvernbright/Desktop/ML/House_Price_Data_Dirty.csv", header = TRUE )
str(df)

summary(df)

hist(df$crime_rate)
pairs(~price+crime_rate+n_hot_rooms+rainfall, data = df)
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
View(df)

df2 <- df[, -7:-10]
df <- df2
rm(df2)
rm(df3)
df <- df[,-14]

df3 <- dummy(df, object = NULL)
df[names(df3)] <- df3
df <- df[,-16]
df <- df[,-19]
df <- df[,-12]
df <- df[,-9]

numeric_df <- df[sapply(df, is.numeric)]
round(cor(numeric_df, use = "complete.obs"), 3)
df <- df[, -4]
df <- df[, -11]
round(cor(numeric_df, use = "complete.obs"), 3)
summary(df)

# LinearRegression
simple_model <- lm(price~room_num, data = df)
summary(simple_model)
plot(df$room_num,df$price)
abline(simple_model)

summary(df$room_num)
summary(df$price)
str(df)
df$log_price <- log(df$price + 1)
log_model <- lm(log_price ~ room_num, data = df)
summary(log_model)
plot(df$room_num, df$log_price, main = "Log(Price) vs Room Num", abline(log_model, col = "red"))
plot(df$crime_rate, df$price)
df$crime_rate <- log(df$crime_rate + 1)
plot(df$n_hos_beds, df$price)
boxplot(df$price, main = "Price Outliers")
df_clean <- subset(df, price < 110)
df_clean$log_price <- log(df_clean$price + 1)
clean_model <- lm(price ~ room_num, data = df_clean)
summary(clean_model)
plot(df_clean$room_num, df_clean$price, main = "Cleaned Data Linear Fit", abline(clean_model, col = "blue"))

