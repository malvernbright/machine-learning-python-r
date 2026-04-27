### ---------------------------------------------------------
### CNN.R - Fashion MNIST Classification (CachyOS R 4.6 Fix)
### ---------------------------------------------------------

# 1. ENVIRONMENT SETUP
# ---------------------------------------------------------
# Your .Renviron already points to /home/malve/miniconda3/envs/tf_env/bin/python
install.packages("reticulate")
install.packages("keras3")
library(reticulate)
library(keras3)
#install_keras()

# 2. DATA PREPARATION
# ---------------------------------------------------------
fashion_mnist <- dataset_fashion_mnist()

# Test Train Split
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

# Preprocess: Normalize pixel values (0 to 1)
train_images <- train_images / 255
test_images <- test_images / 255

class_names <- c(
  'T-shirt/top', 'Trouser', 'Pullover', 'Dress', 'Coat', 
  'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot'
)

# Set up validation and partial training sets
val_indices <- 1:5000
val_images <- train_images[val_indices, , ]
val_labels <- train_labels[val_indices]
part_train_images <- train_images[-val_indices, , ]
part_train_labels <- train_labels[5001:60000]

# Reshape for CNN (Samples, Width, Height, Channels)
part_train_images <- array_reshape(part_train_images, c(55000, 28, 28, 1))
val_images <- array_reshape(val_images, c(5000, 28, 28, 1))
test_images <- array_reshape(test_images, c(10000, 28, 28, 1))

# 3. MODEL ARCHITECTURE
# ---------------------------------------------------------
model <- keras_model_sequential() %>%
  layer_conv_2d(
    filters = 32, 
    kernel_size = c(3, 3), 
    activation = "relu", 
    input_shape = c(28, 28, 1)
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 300, activation = "relu") %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

# 4. COMPILATION AND TRAINING
# ---------------------------------------------------------
model %>% compile(
  optimizer = 'sgd',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

# Training on your tf_env
model %>% fit(
  part_train_images, 
  part_train_labels, 
  epochs = 30, 
  batch_size = 63,
  validation_data = list(val_images, val_labels)
)

# 5. EVALUATION
# ---------------------------------------------------------
CNN_Score <- model %>% evaluate(test_images, test_labels)
cat('\nFinal Test Accuracy:', CNN_Score$accuracy, "\n")