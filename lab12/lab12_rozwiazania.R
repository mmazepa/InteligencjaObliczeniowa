# LABORATORIUM 12 [08.01.2019]
# ------------------------------------------------------------------------

# ___ TUTORIAL 1 _________________________________________________________
#
# Deep Learning for Text Classification with Keras
# https://blogs.rstudio.com/tensorflow/posts/2017-12-07-text-classification-with-keras/
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
#
# Deep Learning with R
# https://www.manning.com/books/deep-learning-with-r
#
# Chapter 6. Deep learning for text and sequences
# 6.1. Working with text data 
# https://livebook.manning.com/#!/book/deep-learning-with-r/chapter-6/
# ________________________________________________________________________

# --- WORD-LEVEL ONE-HOT ENCODING (TOY EXAMPLE) --------------------------

samples <- c("The cat sat on the mat.",
             "The dog ate my homework.")
token_index <- list()

for (sample in samples)
  for (word in strsplit(sample, " ")[[1]])
    if (!word %in% names(token_index))
      token_index[[word]] <- length(token_index) + 2

max_length <- 10
results <- array(0, dim = c(length(samples),
                            max_length,
                            max(as.integer(token_index))))

for (i in 1:length(samples)) {
  sample <- samples[[i]]
  words <- head(strsplit(sample, " ")[[1]], n = max_length)
  for (j in 1:length(words)) {
    index <- token_index[[words[[j]]]]
    results[[i, j, index]] <- 1
  }
}

# --- CHARACTER-LEVEL ONE-HOT ENCODING (TOY EXAMPLE) ---------------------

samples <- c("The cat sat on the mat.",
             "The dog ate my homework.")
ascii_tokens <- c("", sapply(as.raw(c(32:126)), rawToChar))
token_index <- c(1:(length(ascii_tokens)))
names(token_index) <- ascii_tokens

max_length <- 50
results <- array(0, dim = c(length(samples), max_length, length(token_index)))

for (i in 1:length(samples)) {
  sample <- samples[[i]]
  characters <- strsplit(sample, "")[[1]]
  for (j in 1:length(characters)) {
    character <- characters[[j]]
    results[i, j, token_index[[character]]] <- 1
  }
}

# --- USING KERAS FOR WORD-LEVEL ONE-HOT ENCODING ------------------------

library(keras)
samples <- c("The cat sat on the mat.",
             "The dog ate my homework.")

tokenizer <- text_tokenizer(num_words = 1000) %>%
  fit_text_tokenizer(samples)

sequences <- texts_to_sequences(tokenizer, samples)
one_hot_results <- texts_to_matrix(tokenizer, samples, mode = "binary")
word_index <- tokenizer$word_index

cat("Found", length(word_index), "unique tokens.\n")

# --- WORD-LEVEL ONE-HOT ENCODING WITH HASHING TRICK (TOY EXAMPLE) -------

#install.packages("hashFunction")
library(hashFunction)
samples <- c("The cat sat on the mat.",
             "The dog ate my homework.")
dimensionality <- 1000

max_length <- 10
results <- array(0, dim = c(length(samples), max_length, dimensionality))

for (i in 1:length(samples)) {
  sample <- samples[[i]]
  words <- head(strsplit(sample, " ")[[1]], n = max_length)
  for (j in 1:length(words)) {
    index <- abs(spooky.32(words[[i]])) %% dimensionality
    results[[i, j, index]] <- 1
  }
}

# --- INSTANTIATING AN EMBEDDING LAYER -----------------------------------

embedding_layer <- layer_embedding(input_dim = 1000, output_dim = 64)

# --- LOADING THE IMDB DATA FOR USE WITH EMBEDDING LAYER -----------------

max_features <- 10000
maxlen <- 20

imdb <- dataset_imdb(num_words = max_features)

c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb

x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test <- pad_sequences(x_test, maxlen = maxlen)

# --- USING AN EMBEDDING LAYER AND CLASSIFIER ON THE IMDB DATA -----------

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 8,
                  input_length = maxlen) %>%
  layer_flatten() %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

summary(model)

history <- model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2
)

# --- PROCESSING THE LABELS OF THE RAW IMDB DATA -------------------------

# Nale¿y pobraæ plik ze strony: http://mng.bz/0tIo

imdb_dir <- "C:/Users/Mariusz/Desktop/IO/Lab Repo/lab12/aclImdb"
train_dir <- file.path(imdb_dir, "train")
labels <- c()
texts <- c()

for (label_type in c("neg", "pos")) {
  label <- switch(label_type, neg = 0, pos = 1)
  dir_name <- file.path(train_dir, label_type)
  for (fname in list.files(dir_name, pattern = glob2rx("*.txt"),
                           full.names = TRUE)) {
    texts <- c(texts, readChar(fname, file.info(fname)$size))
    labels <- c(labels, label)
  }
}

# --- TOKENIZING THE DATA ------------------------------------------------

library(keras)
maxlen <- 100
training_samples <- 500
validation_samples <- 10000
max_words <- 10000
tokenizer <- text_tokenizer(num_words = max_words) %>%
  fit_text_tokenizer(texts)
sequences <- texts_to_sequences(tokenizer, texts)
word_index = tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")

data <- pad_sequences(sequences, maxlen = maxlen)
labels <- as.array(labels)
cat("Shape of data tensor:", dim(data), "\n")
cat('Shape of label tensor:', dim(labels), "\n")

indices <- sample(1:nrow(data))
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1):
                                (training_samples + validation_samples)]

x_train <- data[training_indices,]
y_train <- labels[training_indices]
x_val <- data[validation_indices,]
y_val <- labels[validation_indices]

# --- PARSING THE GLOVE WORD-EMBEDDINGS FILE -----------------------------

# Nale¿y pobraæ plik ze strony: http://nlp.stanford.edu/data/glove.6B.zip

glove_dir = "C:/Users/Mariusz/Desktop/IO/Lab Repo/lab12/glove.6B"
lines <- readLines(file.path(glove_dir, "glove.6B.100d.txt"))
embeddings_index <- new.env(hash = TRUE, parent = emptyenv())

for (i in 1:length(lines)) {
  line <- lines[[i]]
  values <- strsplit(line, " ")[[1]]
  word <- values[[1]]
  embeddings_index[[word]] <- as.double(values[-1])
}

cat("Found", length(embeddings_index), "word vectors.\n")

# --- PREPARING THE GLOVE WORD-EMBEDDINGS MATRIX -------------------------

embedding_dim <- 100
embedding_matrix <- array(0, c(max_words, embedding_dim))

for (word in names(word_index)) {
  index <- word_index[[word]]
  if (index < max_words) {
    embedding_vector <- embeddings_index[[word]]
    if (!is.null(embedding_vector))
      embedding_matrix[index+1,] <- embedding_vector
  }
}

# --- MODEL DEFINITION ---------------------------------------------------

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words, output_dim = embedding_dim,
                  input_length = maxlen) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

# --- LOADING PRETRAINED WORD EMBEDDINGS INTO EMBEDDING LAYER ------------

get_layer(model, index = 1) %>%
  set_weights(list(embedding_matrix)) %>%
  freeze_weights()

# --- TRAINING AND EVALUATION --------------------------------------------

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

setwd("C:/Users/Mariusz/Desktop/IO/Lab Repo/lab12/")
save_model_weights_hdf5(model, "pre_trained_glove_model.h5")

# --- PLOTTING THE RESULTS -----------------------------------------------

plot(history)

# --- TRAINING THE SAME MODEL WITHOUT PRETRAINED WORD EMBEDDINGS ---------

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words, output_dim = embedding_dim,
                  input_length = maxlen) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

# --- TOKENIZING THE DATA OF THE TEST SET --------------------------------

test_dir <- file.path(imdb_dir, "test")
labels <- c()
texts <- c()

for (label_type in c("neg", "pos")) {
  label <- switch(label_type, neg = 0, pos = 1)
  dir_name <- file.path(test_dir, label_type)
  for (fname in list.files(dir_name, pattern = glob2rx("*.txt"),
                           full.names = TRUE)) {
    texts <- c(texts, readChar(fname, file.info(fname)$size))
    labels <- c(labels, label)
  }
}

sequences <- texts_to_sequences(tokenizer, texts)
x_test <- pad_sequences(sequences, maxlen = maxlen)
y_test <- as.array(labels)

# --- EVALUATING THE MODEL OF THE TEST SET -------------------------------

model %>%
  load_model_weights_hdf5("pre_trained_glove_model.h5") %>%
  evaluate(x_test, y_test)

# ------------------------------------------------------------------------