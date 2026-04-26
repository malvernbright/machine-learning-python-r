library(reticulate)
use_virtualenv("r-keras", required = TRUE)

library(keras3)
install.packages("tfdatasets")
library(tfdatasets)
# Creating the directories

#CNN_DIR <- "./CNN"
#dir.create(CNN_DIR)
base_dir <- "datafiles/cats_and_dogs_small"

train_dir <- file.path(base_dir, "train")
validation_dir <- file.path(base_dir, "validation")
test_dir <- file.path(base_dir, "test")

# Data augmentation layer (define once)
data_augmentation <- keras_model_sequential() %>%
  layer_random_flip("horizontal") %>%
  layer_random_rotation(0.1) %>%
  layer_random_zoom(0.1) %>%
  layer_random_translation(height_factor = 0.1, width_factor = 0.1)

# Training dataset with augmentation + performance optimizations
train_generator <- image_dataset_from_directory(
  train_dir,
  image_size = c(150, 150),
  batch_size = 20,
  label_mode = "binary",
  shuffle = TRUE
) %>%
  dataset_map(function(images, labels) {
    list(data_augmentation(images, training = TRUE), labels)
  }, num_parallel_calls = tf$data$AUTOTUNE) %>%
  dataset_prefetch(buffer_size = tf$data$AUTOTUNE)

# Validation dataset — NO augmentation, but still prefetch for speed
validation_generator <- image_dataset_from_directory(
  validation_dir,
  image_size = c(150, 150),
  batch_size = 20,
  label_mode = "binary",
  shuffle = FALSE
) %>%
  dataset_prefetch(buffer_size = tf$data$AUTOTUNE)

# Model Architecture
model <- keras_model_sequential() %>%
  layer_conv_2d(
    filters = 32,
    kernel_size = c(3, 3),
    activation = "relu",
    input_shape = c(150, 150, 3)
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3, 3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128,
                kernel_size = c(3, 3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128,
                kernel_size = c(3, 3),
                activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")


model


# Compiling the model
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(learning_rate = 1e-4),
  metrics = c("acc")
)

# Fitting the model with fit function
# steps_per_epoch controls the infinite loop
history <- model %>% fit(
  train_generator,
  steps_per_epoch = 100,
  epochs = 20,
  validation_data = validation_generator,
  validation_steps = 50
)

model %>% save_model("Projects/cats_and_dogs_small_2_r.keras")

# Displaying curves of loss and accuracy during training
plot(history)

