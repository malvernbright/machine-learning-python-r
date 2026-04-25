# install keras r packages
install.packages("keras3")
library(keras3)
install_keras()
# install_keras(tensorflow = "gpu")

fashion_mnist <- dataset_fashion_mnist()
View(fashion_mnist)
# Test-Train Split
#train_images <- fashion_mnist$train$x
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

# Explore data structure
dim(train_images) # dimension
str(train_images) # structure

# Plotting the image
fobject <- train_images[11, , ]
plot(as.raster(fobject, max = 255)) # raster is a pixelated image

class_names <- c(
  'T-shirt/top',
  'Trouser',
  'Pullover',
  'Dress',
  'Coat',
  'Sandal',
  'Shirt',
  'Sneaker',
  'Bag',
  'Ankle boot'
)
class_names[train_labels[11] + 1]

# Normalizing [(X-mean)/std.Dev]

train_images <- train_images / 255
test_images <- test_images / 255

# Creating a validation split - used for hyperparameter tuning
val_indices <- 1:5000
val_images <- train_images[-val_indices,,]
val_labels <- train_labels[val_indices]
part_train_labels <- train_labels[-val_indices]
