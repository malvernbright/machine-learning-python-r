# ========================================================
# 1. LOAD DATA & FILL MISSING VALUES
# ========================================================
df <- read.csv("/Users/malvernbright/Desktop/ML/House_Price_Data_Dirty.csv", header = TRUE)

# Fill Missing room_num with Mean
df$room_num[is.na(df$room_num)] <- mean(df$room_num, na.rm = TRUE)

# ========================================================
# 2. OUTLIER TREATMENT: THE IQR METHOD
# ========================================================
# The 99th percentile failed because the outliers made up >1% of data.
# The IQR method calculates the "Normal Range" mathematically.

# Step A: Calculate Quartiles
Q1 <- quantile(df$price, 0.25) # The 25% mark
Q3 <- quantile(df$price, 0.75) # The 75% mark
IQR_value <- Q3 - Q1           # The range of the middle 50%

print(paste("Normal IQR Range:", round(IQR_value, 2)))

# Step B: Define the "Whiskers" (Upper Limit)
# Standard rule: Upper Limit = Q3 + 1.5 * IQR
# Any value above this is statistically an outlier.
upper_limit <- Q3 + (1.5 * IQR_value)

print(paste("Old 99th Percentile Limit:", round(quantile(df$price, 0.99), 2)))
print(paste("New IQR Upper Limit:", round(upper_limit, 2)))

# Step C: Visual Proof BEFORE Fix
boxplot(df$price, main = "Price BEFORE IQR Capping", col="orange")
abline(h = upper_limit, col = "blue", lwd = 2, lty = 2) # Show the cut-off line

# Step D: Apply the IQR Cap
# If price > upper_limit, force it down to upper_limit
df$price[df$price > upper_limit] <- upper_limit

# Step E: Visual Proof AFTER Fix
boxplot(df$price, main = "Price AFTER IQR Capping", col="lightgreen")

# ========================================================
# 3. VERIFY LINEAR RELATIONSHIP
# ========================================================
final_model <- lm(price ~ room_num, data = df)

# Check the Summary
# Look for: P-value < 0.05 and R-squared > 0.5
summary(final_model)

# Plot the beautiful linear fit
plot(df$room_num, df$price, 
     main = "Linear Relationship (IQR Method)", 
     xlab = "Room Number", ylab = "Price", 
     pch = 19, col = "darkblue")
abline(final_model, col = "red", lwd = 2)
