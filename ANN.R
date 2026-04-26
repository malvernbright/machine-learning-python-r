# install keras r packages
# install.packages("keras3")
library(keras3)
#install_keras()

# Load Dataset
fashion_mnist <- dataset_fashion_mnist()

# Test-Train Split using multi-assignment operator
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

fobject <- train_images[11, , ]
plot(as.raster(fobject, max = 255))
print(class_names[train_labels[11] + 1])

# Normalizing pixel values to a range of 0 to 1
train_images <- train_images / 255
test_images <- test_images / 255

# --- FIX: CORRECT VALIDATION SPLIT ---
val_indices <- 1:5000

# Select the first 5000 images for validation
val_images <- train_images[val_indices, , ]
val_labels <- train_labels[val_indices]

# Exclude those 5000 images to create the partial training set
part_train_images <- train_images[-val_indices, , ]
part_train_labels <- train_labels[-val_indices]
# -------------------------------------

# Build the Neural Network Architecture
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

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
  batch_size = 100,
  validation_data = list(val_images, val_labels)
)

# Test Performance
score <- model %>% evaluate(test_images, test_labels)

cat('Test loss:', score["loss"], "\n")
cat('Test accuracy:', score["accuracy"], "\n")

# Predicting on Test
predictions <- model %>% predict(test_images)

# --- FIX: REPLACING predict_classes ---
# Use apply to find the index with the maximum probability for each row
class_pred <- apply(predictions, 1, which.max) - 1
# --------------------------------------

cat("First 20 predictions: ", class_pred[1:20], "\n")
cat("Actual class for first test image: ", class_names[class_pred[1] + 1], "\n")
plot(as.raster(test_images[1,,], max = 255))


# NeuralNet Package
#install.packages("neuralnet")
library(neuralnet)

hours <- c(20,10,30,20,50,30)
mocktest <- c(90,20,20,10,50,80)
Passed <- c(1,0,0,0,1,1)
df <- data.frame(hours, mocktest,Passed)
nn <- neuralnet(Passed~hours+mocktest, 
                data = df, hidden = c(3, 2),
                act.fct = "logistic", linear.output = FALSE)

plot(nn)

thours <- c(20,20,30)
tmocktest <- c(80,30,20)
test <- data.frame(thours,tmocktest)
Predict <- compute(nn, test)
Predict$net.result
prob <- Predict$net.result
pred <- ifelse(prob > .5, 1, 0)
pred 



# Regression Neural Network with Functional API

# Loading the inbuilt Dataset
boston_housing <- dataset_boston_housing()
# to know more about the dataset https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

# Test data is *not* used when calculating mean and std

# Normalize trainig data
train_data <- scale(train_data)

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center")
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)


# Functional API has 2 parts: inputs and outputs

# input layer
inputs <- layer_input(shape = dim(train_data)[2])

# outputs compose input + dense layers
predictions <- inputs %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1)

# Create and compile model
model <- keras_model(inputs = inputs, outputs = predictions)
model %>% compile(
  optimizer = 'rmsprop',
  loss = 'mse',
  metrics = list("mean_absolute_error")
)

model %>% fit(train_data, train_labels, epochs = 30, batch_size = 100)


# Test Performance

score <- model %>% evaluate(test_data, test_labels)
cat('Test loss:', score$loss, "\n")
cat('Test absolute error:', score$mean_absolute_error, "\n")



# input layer

inputs_func <- layer_input(shape = dim(train_data)[2])

# Outputs compose input + dense layers
prediction_func <- inputs_func %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu')

# Reusing the input features after the second hidden layer
main_output <- layer_concatenate(prediction_func, inputs_func) %>%
  layer_dense(units = 1)


# Create and compile model
model_func <- keras_model(inputs = inputs_func, outputs = main_output)
model_func %>% compile(
  optimizer = 'rmsprop',
  loss = 'mse',
  metrics = list("mean_absolute_error")
)

summary(model_func)

model_func %>% fit(train_data, train_labels, epochs = 30, batch_size = 100)

# Test Performance
score_func <- model_func %>% evaluate(test_data, test_labels)
cat('Functional Model Test Loss:', score_func$loss, "\n")
cat('Normal Model Test Loss:', score$loss, "\n")
cat('Fuctional Model Test Mean Abs Error:', score_func$mean_absolute_error, "\n")
cat('Normal Model Mean Abs Error:', score$mean_absolute_error, "\n")


#### Saving & Restoring Models ####
#model_func %>% fit(train_data, train_labels, epochs = 30, batch_size = 100)

model_func %>% save_model("my_model.keras")

new_model <- load_model("my_model.keras")

model_func %>% summary()
#summary(model_func)
new_model %>% summary()

### Using callbacks to create epoch store points

checkpoint_dir <- "checkpoints"
dir.create(checkpoint_dir, showWarnings = FALSE)
filepath <- file.path(checkpoint_dir, "Epoch-{epoch:02d}.keras")

# Create checkpoint callback
cp_callback <- callback_model_checkpoint(filepath = filepath)

rm(model_func)
clear_session()

model_callback <- keras_model(inputs = inputs_func, outputs = main_output)
model_callback %>% compile(optimizer = 'rmsprop', loss = 'mse',
                           metrics = list("mean_absolute_error"))

model_callback %>% fit(train_data, train_labels, epochs = 30,
                       callbacks = list(cp_callback))

list.files(checkpoint_dir)

tenth_model <- load_model(file.path(checkpoint_dir, "Epoch-10.keras"))
summary(tenth_model)

### Only Saving the best Model ###

callbacks_best <- callback_model_checkpoint(filepath = "best_model.keras", 
                                            monitor = "val_loss",
                                            save_best_only = TRUE)

rm(model_callback)
clear_session()

model_cb_best <- keras_model(inputs = inputs_func, outputs = main_output)
model_cb_best %>% compile(optimizer = 'rmsprop', loss = 'mse',
                          metrics = list("mean_absolute_error"))
model_cb_best %>% fit(train_data, train_labels, epochs = 30,
                      validation_data = list(test_data, test_labels),
                      callbacks = list(callbacks_best))

best_model <- load_model("best_model.keras")


### Stopping the processing when we find the best model

callbacks_list <- list(
  callback_early_stopping(monitor = "val_loss", patience = 3),
  callback_model_checkpoint(filepath = "best_model_early_stopping.keras", 
                            monitor = "val_loss", save_best_only = TRUE)
)

rm(model_cb_best)
clear_session()

model_cb_early <- keras_model(inputs = inputs_func, outputs = main_output)
model_cb_early %>% compile(optimizer = 'rmsprop', loss = 'mse',
                          metrics = list("mean_absolute_error"))

model_cb_early %>% fit(train_data, train_labels, epochs = 100,
                       validation_data = list(test_data, test_labels),
                       callbacks = callbacks_list)
