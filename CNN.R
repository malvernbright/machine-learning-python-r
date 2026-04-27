# 1. First, tell the system to hide the GPU from TensorFlow
Sys.setenv(CUDA_VISIBLE_DEVICES = "-1")

# 2. Now load the libraries (they will only see the CPU)
library(reticulate)
library(keras3)
library(tensorflow)

# 3. Data Preparation
fashion_mnist <- dataset_fashion_mnist()
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

# Normalize
train_images <- train_images / 255
test_images <- test_images / 255

# Reshape (samples, width, height, channels)
train_images <- array_reshape(train_images, c(60000, 28, 28, 1))
test_images <- array_reshape(test_images, c(10000, 28, 28, 1))

# 4. Define Architecture (Unified Block)
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(28, 28, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu") %>%
  layer_flatten() %>%
  layer_dense(units = 300, activation = "relu") %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

# 5. Compile and Train
model %>% compile(
  optimizer = 'sgd',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

model %>% fit(
  train_images, train_labels,
  epochs = 10,
  batch_size = 64,
  validation_split = 0.2
)

