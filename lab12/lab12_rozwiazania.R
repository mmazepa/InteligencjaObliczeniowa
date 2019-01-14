# LABORATORIUM 12 [08.01.2019]
# ------------------------------------------------------------------------

# ___ TUTORIAL 1 _________________________________________________________
#
# Deep Learning for Text Classification with Keras
# ________________________________________________________________________

devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow(method = "conda", conda = "auto",
                   version = "1.5.0", envname = "r-tensorflow")
library(reticulate)
use_condaenv("r-tensorflow", required = TRUE)
library(keras)
install_keras(method = "conda", tensorflow = "1.5.0")
is_keras_available()

# --- THE IMDB DATASET ---------------------------------------------------

imdb <- dataset_imdb(num_words = 10000)
train_data <- imdb$train$x
train_labels <- imdb$train$y
test_data <- imdb$test$x
test_labels <- imdb$test$y

str(train_data[[1]])
train_labels[[1]]
max(sapply(train_data, max))

# Named list mapping words to an integer index.
word_index <- dataset_imdb_word_index()
reverse_word_index <- names(word_index)
names(reverse_word_index) <- word_index

# Decodes the review. Note that the indices are offset by 3 because 0, 1, and 
# 2 are reserved indices for "padding," "start of sequence," and "unknown."
decoded_review <- sapply(train_data[[1]], function(index) {
  word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
  if (!is.null(word)) word else "?"
})
cat(decoded_review)

# --- PREPARING THE DATA -------------------------------------------------

vectorize_sequences <- function(sequences, dimension = 10000) {
  # Creates an all-zero matrix of shape (length(sequences), dimension)
  results <- matrix(0, nrow = length(sequences), ncol = dimension) 
  for (i in 1:length(sequences))
    # Sets specific indices of results[i] to 1s
    results[i, sequences[[i]]] <- 1 
  results
}

x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)

str(x_train[1,])

y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)

# --- BUILDING YOUR NETWORK ----------------------------------------------

model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

# --- LOSS FUNCTION AND OPTIMIZER ----------------------------------------

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# --- VALIDATING YOUR APPROACH -------------------------------------------

val_indices <- 1:10000

x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

plot(history)

model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results <- model %>% evaluate(x_test, y_test)

results

# --- GENERATING PREDICTIONS ---------------------------------------------

model %>% predict(x_test[1:10,])


# ___ TUTORIAL 2 _________________________________________________________

# ...

# ------------------------------------------------------------------------