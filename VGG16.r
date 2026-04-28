# 1. First, tell the system to hide the GPU from TensorFlow
Sys.setenv(CUDA_VISIBLE_DEVICES = "-1")

# 2. Now load the libraries (they will only see the CPU)
library(reticulate)
library(keras3)
library(tensorflow)

#install.packages("tfdatasets")
library(tfdatasets)
# Creating the directories

#CNN_DIR <- "./CNN"
#dir.create(CNN_DIR)
base_dir <- "datafiles/cats_and_dogs_small"
train_dir <- file.path(base_dir, "train")
validation_dir <- file.path(base_dir, "validation")
test_dir <- file.path(base_dir, "test")

conv_base <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(150, 150, 3)
)

# weights specifies the weight checkpoint from which to initialize the model
# include_top refers to including (or not) the densely connected classifier on top of the network
# input_shape is the shape of the image tensors that you'll feed to the network

conv_base

# Adding our fully connected classifier to base

model <- keras_model_sequential() %>%
  conv_base %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model
freeze_weights(conv_base)
model

# creating batch data generators with data augmentation
# Data augmentation layer (define once)
data_augmentation <- keras_model_sequential() %>%
  layer_random_flip("horizontal") %>%
  layer_random_rotation(0.1) %>%
  layer_random_zoom(0.1) %>%
  layer_random_translation(height_factor = 0.1, width_factor = 0.1)

# Training dataset with augmentation + performance optimizations
train_datagen <- image_dataset_from_directory(
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

# compiling the model - notice the small value of learning rate
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(learning_rate = 2e-5),
  metrics = c("acc")
)

# training the model
history <- model %>% fit(
  train_generator,
  steps_per_epoch = 100,
  epochs = 30,
  validation_data = validation_generator,
  validation_steps = 50
)

save_model(model, "CNN_VGG.keras")

vgg_model <- load_model("CNN_VGG.keras")

# Checking the performance on test set
test_generator <- flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size = c(150, 150),
  label_mode = "binary",
)

last_model %>% evaluate_generator(validation_generator, steps=50)