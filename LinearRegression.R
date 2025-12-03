# ========================================================
# 1. LOAD DATA
# ========================================================
# Replace the path below with your actual file path
df <- read.csv("/Users/malvernbright/Desktop/ML/House_Price_Data_Dirty.csv", header = TRUE)

# Inspect initial state (optional)
print("Initial Summary:")
summary(df$room_num) # Shows NA values
summary(df$price)    # Shows extreme Max value (e.g., 300+)

# ========================================================
# 2. MISSING VALUE TREATMENT (Imputation)
# ========================================================
# Goal: Fill the NA gaps in 'room_num' so the model can use those rows.

# Calculate the mean of the existing data
mean_room <- mean(df$room_num, na.rm = TRUE)

# Replace NA cells with that mean
df$room_num[is.na(df$room_num)] <- mean_room

# Verify NAs are gone
print(paste("Remaining NAs in room_num:", sum(is.na(df$room_num))))

# ========================================================
# 3. OUTLIER TREATMENT (Capping / Winsorization)
# ========================================================
# Goal: Fix the extreme prices that are skewing the slope.
# Instead of deleting rows (subset), we "cap" them at a maximum logical value.

# A. Visualize before fixing
boxplot(df$price, main = "Price BEFORE Capping (Outliers Visible)", col="orange")

# B. Calculate the 99th percentile (The upper limit)
upper_limit <- quantile(df$price, 0.99)
print(paste("Capping values above:", round(upper_limit, 2)))

# C. Apply the Cap
# Logic: Find prices > limit, and set them equal to the limit.
df$price[df$price > upper_limit] <- upper_limit

# D. Visualize after fixing
# The plot should now look reasonable
boxplot(df$price, main = "Price AFTER Capping", col="lightgreen")

# ========================================================
# 4. BUILD & VISUALIZE MODEL
# ========================================================

# Run the Linear Regression on the cleaned data
final_model <- lm(price ~ room_num, data = df)

# Print the key statistics
print("Model Summary:")
summary(final_model)

# Plot the scatter plot with the regression line
plot(df$room_num, df$price, 
     main = "Relationship: Room Num vs Price (Cleaned)", 
     xlab = "Room Number", 
     ylab = "Price", 
     pch = 19,           # Solid circles
     col = "darkblue")   # Blue dots

# Add the red regression line
abline(final_model, col = "red", lwd = 2)
df_clean <- subset(df, price < 110)
df_clean$log_price <- log(df_clean$price + 1)
clean_model <- lm(price ~ room_num, data = df_clean)
summary(clean_model)
plot(df_clean$room_num, df_clean$price, 
     main = "Cleaned Data Linear Fit", 
     xlab = "Room Number", 
     ylab = "Price", 
     pch = 19, 
     abline(clean_model, col = "blue"))

