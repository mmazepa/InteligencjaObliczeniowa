# LABORATORIUM 12 [08.01.2019]
# ------------------------------------------------------------------------

install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")

image_dir <- "C:/Users/Mariusz/Desktop/IO/Lab Repo/lab12/train"

source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)

example_cat_image <- readImage(file.path(image_dir, "Cat/0.jpg"))
display(example_cat_image)
example_dog_image <- readImage(file.path(image_dir, "Dog/0.jpg"))
display(example_dog_image)

# ------------------------------------------------------------------------

width <- 28
height <- 28

install.packages("pbapply")
library(pbapply)

extract_feature <- function(dir_path, width, height, is_cat = TRUE, add_label = TRUE) {
  img_size <- width*height
  images_names <- list.files(dir_path)
  if (add_label) {
    images_names <- images_names[grepl(ifelse(is_cat, "cat", "dog"), images_names)]
    label <- ifelse(is_cat, 0, 1)
  }
  print(paste("Start processing", length(images_names), "images"))
  feature_list <- pblapply(images_names, function(imgname) {
    img <- readImage(file.path(dir_path, imgname))
    img_resized <- resize(img, w = width, h = height)
    grayimg <- channel(img_resized, "gray")
    img_matrix <- grayimg@.Data
    img_vector <- as.vector(t(img_matrix))
    return(img_vector)
  })
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  if (add_label) {
    feature_matrix <- cbind(label = label, feature_matrix)
  }
  return(feature_matrix)
}

cats_data <- extract_feature(dir_path = image_dir, width = width, height = height)
dogs_data <- extract_feature(dir_path = image_dir, width = width, height = height, is_cat = FALSE)
dim(cats_data)
dim(dogs_data)

saveRDS(cats_data, "cat.rds")
saveRDS(dogs_data, "dog.rds")

# ------------------------------------------------------------------------

install.packages("caret")
library(caret)

complete_set <- rbind(cats_data, dogs_data)
training_index <- createDataPartition(complete_set$label, p = .9, times = 1)
training_index <- unlist(training_index)

train_set <- complete_set[training_index,]
dim(train_set)
test_set <- complete_set[-training_index,]
dim(test_set)

train_data <- data.matrix(train_set)
train_x <- t(train_data[, -1])
train_y <- train_data[,1]
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

test_data <- data.matrix(test_set)
test_x <- t(test_set[,-1])
test_y <- test_set[,1]
test_array <- test_x
dim(test_array) <- c(28, 28, 1, ncol(test_x))

install.packages("mxnet")
library(mxnet)

mx_data <- mx.symbol.Variable('data')

conv_1 <- mx.symbol.Convolution(data = mx_data, kernel = c(5, 5), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2,2 ))

conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5,5), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))

flat <- mx.symbol.Flatten(data = pool_2)
fcl_1 <- mx.symbol.FullyConnected(data = flat, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fcl_1, act_type = "tanh")
fcl_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 2)

NN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)

mx.set.seed(100)

device <- mx.cpu()

model <- mx.model.FeedForward.create(NN_model, X = train_array, y = train_y,
                                     ctx = device,
                                     num.round = 30,
                                     array.batch.size = 100,
                                     learning.rate = 0.05,
                                     momentum = 0.9,
                                     wd = 0.00001,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))

predict_probs <- predict(model, test_array)
predicted_labels <- max.col(t(predict_probs)) - 1
table(test_data[, 1], predicted_labels)

sum(diag(table(test_data[, 1], predicted_labels)))/2500

# ------------------------------------------------------------------------