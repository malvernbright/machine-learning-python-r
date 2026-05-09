# %%
import numpy as np
import pandas as pd

# %%
df = pd.read_csv("House_Price.csv", header=0)

df.head()

# %%
df.dropna(inplace=True)
df

# %%
df.shape

# %%
df.head()

# %%
# Dummy variables
df = pd.get_dummies(df, columns=['airport', 'waterbody'], drop_first=True, dtype=int)
del df['bus_ter']
df.head()

# %%
df.info()

# %%
import seaborn as sns
import matplotlib.pyplot as plt

# %%
# Plot crime_rate vs price
sns.jointplot(x='crime_rate', y='price', data=df)

# %%
df['crime_rate'] = np.log(1 + df.crime_rate) # log transformation

# %%
sns.jointplot(x='crime_rate', y='price', data=df)

# %%
df.head()

# %%
sns.jointplot(x='resid_area', y='price', data=df)

# %%
sns.jointplot(x='air_qual', y='price', data=df)

# %%
df.columns

# %%
sns.jointplot(x='room_num', y='price', data=df)

# %%
# Delete useless variables and remain with mean of those variables
df['avg_dist'] = (df.dist1 + df.dist2 + df.dist3 + df.dist4) / 4
del df['dist1']
del df['dist2']
del df['dist3']
del df['dist4']

df.columns

# %%
df.corr()

# %%
sns.jointplot(x='room_num', y='price', data=df)

# %%
df.describe()

# %%
np.percentile(df.n_hot_rooms, [99])

# %%
np.percentile(df.n_hot_rooms, [99])[0]

# %%
# Cap it
uv = np.percentile(df.n_hot_rooms, [99])[0]
df[(df.n_hot_rooms > uv)]

# %%
df.describe()

# %%
df.loc[df.n_hot_rooms > 3*uv, 'n_hot_rooms'] = 3*uv

# %%
df[(df.n_hot_rooms > uv)]

# %%
lv = np.percentile(df.rainfall, [1])[0]
df[(df.rainfall < lv)]

# %%
df.loc[df.rainfall < .3*lv, 'rainfall'] = .3*lv
df[(df.rainfall < lv)]

# %%
df.describe()

# %%
plt.figure(figsize=(6, 4))
sns.boxplot(y=df['price'])
plt.title("Price BEFORE Capping")
plt.show()

# %%
# Define a function to remove outliers
def remove_outliers(data, column):
    # Calculate IQR
    Q1 = data[column].quantile(0.25)
    Q3 = data[column].quantile(0.75)
    IQR = Q3 - Q1
    # Create a mask for the values that fall outside the IQR
    lower_bound = Q1 - 1.5 * IQR
    upper_bound = Q3 + 1.5 * IQR
    
    # Filter out the data points that are not within the IQR range
    # df = df[(df[column] >= lower_bound) & (df[column] <= upper_bound)]
    data.loc[data[column] > upper_bound, column] = upper_bound
    data.loc[data[column] < lower_bound, column] = lower_bound
    
    return data

# %%
# Step A: Visualizing before (optional)
# plt.figure(figsize=(6, 4))
# sns.boxplot(y=df['price'])
# plt.title("Price BEFORE Capping")
# plt.show()

# Step B: Calculate Quartiles and IQR
# Q1 = df['price'].quantile(0.25)
# Q3 = df['price'].quantile(0.75)
# IQR = Q3 - Q1

# print(f"Q1: {Q1}, Q3: {Q3}, IQR: {IQR}")

# # Step C: Define Upper Limit
# # Formula: Q3 + 1.5 * IQR
# upper_limit = Q3 + (1.5 * IQR)
# lower_limit = Q1 - (1.5 * IQR) # (Optional, if you had low outliers)

# print(f"Capping values above: {upper_limit}")

# # Step D: Apply Capping (Winsorization)
# # Logic: Locate rows where price > upper_limit, and set them to upper_limit
# df.loc[df['price'] > upper_limit, 'price'] = upper_limit
# # (Optional) Cap lower limit too
# df.loc[df['price'] < lower_limit, 'price'] = lower_limit

remove_outliers(data=df, column='price')

# Step E: Visualizing after
plt.figure(figsize=(6, 4))
sns.boxplot(y=df['price'])
plt.title("Price AFTER IQR Capping")
plt.show()

# %%
sns.jointplot(x='n_hot_rooms', y='price', data=df)

# %%
remove_outliers(data=df, column='n_hot_rooms')
sns.jointplot(x='n_hot_rooms', y='price', data=df)

# %%
# df.loc[df.n_hot_rooms > 3*uv, 'n_hot_rooms'] = 3*uv
df[(df.n_hot_rooms > uv)]

# %%
plt.figure(figsize=(6, 4))
sns.boxplot(y=df['n_hot_rooms'])
plt.title("n_hot_rooms AFTER IQR Capping")
plt.show()

# %%
import seaborn as sns

# Histogram of a column
df['n_hot_rooms'].hist(bins=30)

# Scatter plot between two columns
sns.scatterplot(x='n_hot_rooms', y='price', data=df)

# %%
df.describe()

# %%
remove_outliers(data=df, column='age')
df.describe()

# %%
sns.jointplot(data=df, x='age', y='price')

# %%
df.head()

# %%
# Feature Engineering
df['resid_area_air_qual'] = df['air_qual'] + df['resid_area']

df.head()

# %%
from sklearn.preprocessing import StandardScaler

scaler = StandardScaler()
df[['resid_area_air_qual', 'room_num']] = scaler.fit_transform(df[['resid_area_air_qual', 'room_num']])


# %%
from sklearn.model_selection import train_test_split

X = df.drop('price', axis=1)
y = df['price']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)


# %%
from sklearn.ensemble import RandomForestClassifier

model = RandomForestClassifier(n_estimators=100)


# %%
model.fit(X_train, y_train)

# %%



