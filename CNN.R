### Install Keras
#install.packages("keras3")
library(keras3)

# import data
fashion_mnist <- dataset_fashion_mnist()

# Test Train Split
#train_images <- fashion_mnist$train$x
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

# Explore data structure
dim(train_images)
str(train_images)

# Plotting a sample image (index 11)
class_names <- c(
  'T-shirt/top', 'Trouser', 'Pullover', 'Dress', 'Coat', 
  'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot'
)


# Preprocess the data
# Normalizing pixel values to a range of 0 to 1
train_images <- train_images / 255
test_images <- test_images / 255

val_indices <- 1:5000
# Select the first 5000 images for validation
val_images <- train_images[val_indices, , ]
val_labels <- train_labels[val_indices]

# Exclude those 5000 images to create the partial training set
part_train_images <- train_images[-val_indices, , ]
part_train_labels <- train_labels[5001:60000]

str(part_train_images)
part_train_images <- array_reshape(part_train_images, c(55000, 28, 28, 1)) # 1 channel
val_images <- array_reshape(val_images, c(5000, 28, 28, 1)) # 1 channel
test_images <- array_reshape(test_images, c(10000, 28, 28, 1)) # 1 channel

# Define model Architecture
# Define model Architecture
model <- keras_model_sequential() %>%
  layer_conv_2d(
    filters = 32, 
    kernel_size = c(3, 3), 
    activation = "relu",
    input_shape = c(28, 28, 1) # Note: CNNs expect (width, height, channels)
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2))
#%>% layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu")

model <- model %>%
  layer_flatten() %>%
  layer_dense(units = 300, activation = "relu") %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

model


# Configuring the model
# Compile the Model
model %>% compile(
  optimizer = 'sgd',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

# Train the Model
model %>% fit(
  part_train_images, 
  part_train_labels, 
  epochs = 30, 
  batch_size = 63,
  validation_data = list(val_images, val_labels)
)

# Test Performance
CNN_Score <- model %>% evaluate(test_images, test_labels)

cat('Test loss:', CNN_Score$loss, "\n")
cat('Test accuracy:', CNN_Score$acc, "\n")


# Predicting on Test Set
class_pred <- model %>% predict(test_images)
class_pred[1:20]
class_names[class_pred[1:20]+1]
class_names[test_labels[1:20]+1]
plot(as.raster(test_images[13, , ,]), max = 255)
