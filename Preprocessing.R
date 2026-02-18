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
View(df)

df2 <- df[, -7:-10]
df <- df2
rm(df2)
rm(df3)
df <- df[,-14]

#df <- dummy_cols(df, select_columns = c("airport", "waterbody"))
df <- dummy_cols(df, select_columns = c("airport", "waterbody"), remove_selected_columns = TRUE)
df$waterbody_lake_and_river <- df$`waterbody_Lake and River`
df$`waterbody_Lake and River` <- NULL
summary(df)
cor(df)
round(cor(df), 2)
df <- df[,-12] # Remove Parks
