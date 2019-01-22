# ------------------------------------------------------------------------
# PRACA DOMOWA 4
# ------------------------------------------------------------------------
# IMIÊ I NAZWISKO:   Mariusz Mazepa
# NUMER INDEKSU:     235371
# PRZEDMIOT:         Inteligencja Obliczeniowa
# GRUPA ÆWICZENIOWA: 3
# ZAGADNIENIE:       Przetwarzanie Tekstu
# BAZA DANYCH:       YouTube Spam Collection
# LINK DO BAZY:      https://archive.ics.uci.edu/ml/datasets/YouTube+Spam+Collection
# ------------------------------------------------------------------------

# This corpus has been collected using the YouTube Data API v3.

# ----- MODYFIKACJA PLIKÓW BAZOWYCH --------------------------------------

library(readr)
library(dplyr)

originalPath <- file.path("C:", "Users", "Mariusz", "Desktop", "IO",
                          "Lab Repo", "pd4", "YouTube-Spam-Collection-v1")
modifiedPath <- file.path("C:", "Users", "Mariusz", "Desktop", "IO",
                          "Lab Repo", "pd4", "YouTube-Spam-Modified")
dir(originalPath)

prepareFile <- function(filename) {
  loadFile <- function(filename) {
    desiredFile <- paste(originalPath, filename, sep = "/")
    return(read_csv(desiredFile, col_names = TRUE))
  }
  result <- loadFile(filename)
  result <- select(result, CONTENT)
  desiredSavePath <- paste(modifiedPath, filename, sep = "/")
  write.csv(result, file = desiredSavePath, row.names=FALSE)
}

prepareFile("Youtube01-Psy.csv")
prepareFile("Youtube02-KatyPerry.csv")
prepareFile("Youtube03-LMFAO.csv")
prepareFile("Youtube04-Eminem.csv")
prepareFile("Youtube05-Shakira.csv")

# ----- ZA£ADOWANIE TEKSTÓW ----------------------------------------------

# Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud",
#             "biclust", "cluster", "igraph", "fpc")
# install.packages(Needed, dependencies = TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/",
#                  type = "source")

library(tm)

comments <- VCorpus(DirSource(modifiedPath))
summary(comments)
inspect(comments[1])
writeLines(as.character(comments[1]))

# ----- PREPROCESSING ----------------------------------------------------

library(stringr)

preprocessing <- function() {
  comments <- tm_map(comments, tolower)
  comments <- tm_map(comments, removeWords, stopwords("english"))   
  
  for (j in seq(comments)) {
    comments[[j]] <- gsub("/", " ", comments[[j]])
    comments[[j]] <- gsub("@", " ", comments[[j]])
    comments[[j]] <- gsub("\\|", " ", comments[[j]])
    comments[[j]] <- gsub("\u2028", " ", comments[[j]])
    comments[[j]] <- gsub("http\\S+\\s*", " ", comments[[j]])
    comments[[j]] <- gsub("[[:digit:]]", " ", comments[[j]])
    comments[[j]] <- gsub("[[:punct:]]", " ", comments[[j]])
    comments[[j]] <- str_squish(comments[[j]])
  }
  
  comments <- tm_map(comments, removeWords,
                     c("feff", "www", "com", "amp"))
  
  #comments <- tm_map(comments, stemDocument)
  comments <- tm_map(comments, stripWhitespace)
  comments <- tm_map(comments, PlainTextDocument)  
}

tmpComments <- tm_map(comments, stemDocument)
comments[[1]]$content[113]
tmpComments[[1]]$content[113]

comments <- preprocessing()

# ----- TWORZENIE DANYCH DO ANALIZY --------------------------------------

dtm <- DocumentTermMatrix(comments)
dtm

tdm <- TermDocumentMatrix(comments)
tdm

# ----- EKSPLORACJA DANYCH -----------------------------------------------

findFreqTerms(dtm, lowfreq=100)

freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)

m <- as.matrix(dtm)   
dim(m)

saveFile <- function() {
  projectPath <- file.path("C:", "Users", "Mariusz",
                           "Desktop", "IO", "Lab Repo", "pd4")
  projectPath <- paste(projectPath, "DocumentTermMatrix.csv", sep = "/")
  write.csv(m, file=projectPath)  
}

saveFile()

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf <- data.frame(word=names(freq), freq=freq)

head(wf, 10)
head(table(freq), 20)
tail(table(freq), 20)

dtms <- removeSparseTerms(dtm, 0.2)

# ----- WYKRESY: CZÊSTOTLIWOŒÆ WYSTÊPOWANIA S£ÓW -------------------------

library(ggplot2)
p <- ggplot(subset(wf, freq>100),
            aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(text = element_text(size=18),
        axis.text.x=element_text(angle=90, hjust=1)) +
  ggtitle("Word Frequencies") +
  labs(x="words", y="frequency")
p

# ----- ZWI¥ZKI POMIÊDZY TERMINAMI ---------------------------------------

findAssocs(dtm, c("subscribe" , "check"), corlimit=0.85)
findAssocs(dtms, "please", corlimit=0.70)

# ----- WORD CLOUDS ------------------------------------------------------

#install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

set.seed(142)
wordcloud(names(freq), freq, min.freq=50)
wordcloud(names(freq), freq, max.words=100)

wordcloud(names(freq), freq, min.freq=50,
          scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

# ----- KLASTROWANIE WED£UG PODOBIEÑSTWA TEKSTÓW -------------------------

dtmss <- removeSparseTerms(dtms, 0.01)
dtmss

library(cluster)
d <- dist(t(dtmss), method="euclidian")
fit <- hclust(d=d, method="complete")
fit

# install.packages("ape")
library(ape)
colors = c("purple", "orange", "cyan", "red", "green", "blue",
           "darkred", "darkgreen", "dimgray", "black")
clus10 = cutree(fit, 10)
plot(as.phylo(fit), type = "fan", tip.color = colors[clus10],
     label.offset = 1, cex = 0.6)

plot(fit, hang=-1, cex = 0.75)
groups <- cutree(fit, k=6)
rect.hclust(fit, k=6, border="red")

library(fpc)
cluster_amount <- 3
d <- dist(t(dtmss), method="euclidian")
kfit <- kmeans(d, cluster_amount)
clusplot(as.matrix(d), kfit$cluster, color=T,
         shade=T, labels=cluster_amount, lines=0)

# ------------------------------------------------------------------------
# ----- KLASYFIKACJA TEKSTÓW ---------------------------------------------
# ------------------------------------------------------------------------

prepareForClassification <- function() {
  loadFile <- function(filename) {
    desiredFile <- paste(originalPath, filename, sep = "/")
    return(read_csv(desiredFile, col_names = TRUE))
  }
  psy <- loadFile("Youtube01-Psy.csv")
  katyperry <- loadFile("Youtube02-KatyPerry.csv")
  lmfao <- loadFile("Youtube03-LMFAO.csv")
  eminem <- loadFile("Youtube04-Eminem.csv")
  shakira <- loadFile("Youtube05-Shakira.csv")
  result <- rbind(psy, katyperry, lmfao, eminem, shakira)
  result <- select(result, CONTENT, CLASS)
  return (result)
}

db <- data.frame(prepareForClassification())

for (i in 1:nrow(db)) {
  if (db$CLASS[i] == 1) db$CLASS[i] = "spam"
  else db$CLASS[i] = "not-spam"
}

db$CLASS <- factor(db$CLASS)
str(db$CLASS)
table(db$CLASS)

#install.packages("tm")
library(tm)

db_corpus <- VCorpus(VectorSource(db$CONTENT))
print(db_corpus)

inspect(db_corpus[1:3])
as.character(db_corpus[[341]])

db_corpus_clean <- tm_map(db_corpus, content_transformer(tolower))
as.character(db_corpus[[341]])
as.character((db_corpus_clean[[341]]))

db_corpus_clean <- tm_map(db_corpus_clean, removeNumbers)
db_corpus_clean <- tm_map(db_corpus_clean, removeWords, stopwords())
db_corpus_clean <- tm_map(db_corpus_clean, removePunctuation)
as.character((db_corpus_clean[[341]]))

#install.packages("SnowballC")
library(SnowballC)

#db_corpus_clean <- tm_map(db_corpus_clean, stemDocument)
db_corpus_clean <- tm_map(db_corpus_clean, stripWhitespace)
as.character(db_corpus_clean[[341]])

db_corpus_clean <- tm_map(db_corpus_clean, stripWhitespace)
as.character(db_corpus_clean[[341]])

db_dtm <- DocumentTermMatrix(db_corpus_clean)

nrow(db_dtm)
.75 * nrow(db_dtm)
.25 * nrow(db_dtm)

db_dtm_train <- db_dtm[1:1467, ]
db_dtm_test <- db_dtm[1467:1956, ]
db_train_labels <- db[1:1467, ]$CLASS
db_test_labels <- db[1467:1956, ]$CLASS

prop.table(table(db_train_labels))
prop.table(table(db_test_labels))

db_freq_words <- findFreqTerms(db_dtm_train, 5)
str(db_freq_words)

db_dtm_freq_train <- db_dtm_train[ , db_freq_words]
db_dtm_freq_test <- db_dtm_test[ , db_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

db_train <- apply(db_dtm_freq_train, MARGIN = 2, convert_counts)
db_test <- apply(db_dtm_freq_test, MARGIN = 2, convert_counts)

library(e1071)

db_classifier <- naiveBayes(db_train, db_train_labels)
db_test_pred <- predict(db_classifier, db_test)

library(gmodels)
CrossTable(db_test_pred, db_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

(28+5)/489
(252+204)/489

# ----- SENTIMENT ANALYSIS -----------------------------------------------

#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("stringr")
library(tidyverse)
library(tidytext)
library(stringr)
#sentiments
#get_sentiments("afinn")
#get_sentiments("bing")
#get_sentiments("nrc")

comments_words <- data_frame(file = paste0(modifiedPath, 
                                           c("/Youtube01-Psy.csv",
                                             "/Youtube02-KatyPerry.csv",
                                             "/Youtube03-LMFAO.csv",
                                             "/Youtube04-Eminem.csv",
                                             "/Youtube05-Shakira.csv"))) %>%
  mutate(text = map(file, read_lines))
comments_words

comments_words <- comments_words %>%
  unnest() %>%
  filter(!str_detect(text, "^(\\\\[A-Z,a-z])"),
         text != "") %>%
  mutate(line_number = 1:n(),
         file = str_sub(basename(file), 1, -5))

comments_words$file <- forcats::fct_relevel(comments_words$file, c("Youtube01-Psy",
                                                                   "Youtube02-KatyPerry",
                                                                   "Youtube03-LMFAO",
                                                                   "Youtube04-Eminem",
                                                                   "Youtube05-Shakira"))
comments_words <- comments_words %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "[0-9]"),
         word != "ffef",
         word != "www",
         word != "com",
         word != "amp",
         word != "https", 
         word != "http",
         !str_detect(word, "[a-z]_"),
         !str_detect(word, ":"))
comments_words

#comments_words$word <- stemDocument(comments_words$word)
#comments_words

comments_words %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(index = line_number %/% 25, file, sentiment) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = index, y = n, fill = file)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  facet_wrap(~ sentiment, ncol = 5)

comments_words %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(index = line_number %/% 25, file, sentiment) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = index, y = n, fill = file)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  facet_wrap(~ sentiment, ncol = 5)

library(reshape2)
comments_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkgreen"), max.words = 100)

# ------------------------------------------------------------------------

#install.packages("udpipe")
library(udpipe)
udmodel <- udpipe_download_model(language="english")
udmodel

lemma <- udpipe(c(comments[[1]]$content, comments[[2]]$content,
                  comments[[3]]$content, comments[[4]]$content,
                  comments[[5]]$content), object = udmodel)

index <- grep("update", lemma$token)[1]
c(lemma$token[index], lemma$lemma[index])

index <- grep("everyone", lemma$token)[1]
c(lemma$token[index], lemma$lemma[index])

index <- grep("please", lemma$token)[1]
c(lemma$token[index], lemma$lemma[index])

index <- grep("subscribe", lemma$token)[1]
c(lemma$token[index], lemma$lemma[index])

udpipe("learn learned learning learns learner",
       object = udmodel)[9:10]

data.frame("word" = lemma[[10]]) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkgreen"), max.words = 100)

# ------------------------------------------------------------------------

spam <- db[db$CLASS == "spam", ]
notspam <- db[db$CLASS == "not-spam", ]

spam_corpus <- VCorpus(VectorSource(spam$CONTENT))
notspam_corpus <- VCorpus(VectorSource(notspam$CONTENT))

spam_corpus_clean <- tm_map(spam_corpus, content_transformer(tolower))
spam_corpus_clean <- tm_map(spam_corpus_clean, removeNumbers)
spam_corpus_clean <- tm_map(spam_corpus_clean, removeWords, stopwords("en"))
spam_corpus_clean <- tm_map(spam_corpus_clean, removePunctuation)
spam_corpus_clean <- tm_map(spam_corpus_clean, stripWhitespace)

notspam_corpus_clean <- tm_map(notspam_corpus, content_transformer(tolower))
notspam_corpus_clean <- tm_map(notspam_corpus_clean, removeNumbers)
notspam_corpus_clean <- tm_map(notspam_corpus_clean, removeWords, stopwords("en"))
notspam_corpus_clean <- tm_map(notspam_corpus_clean, removePunctuation)
notspam_corpus_clean <- tm_map(notspam_corpus_clean, stripWhitespace)

spam_dtm <- DocumentTermMatrix(spam_corpus_clean)
notspam_dtm <- DocumentTermMatrix(notspam_corpus_clean)

spam_dtm
notspam_dtm

#findFreqTerms(spam_dtm, lowfreq=125)
#findFreqTerms(notspam_dtm, lowfreq=125)

spam_freq <- sort(colSums(as.matrix(spam_dtm)), decreasing=TRUE)
notspam_freq <- sort(colSums(as.matrix(notspam_dtm)), decreasing=TRUE)

spam_wf <- data.frame(word=names(spam_freq), freq=spam_freq)
notspam_wf <- data.frame(word=names(notspam_freq), freq=notspam_freq)

head(spam_wf, 5)[2]
head(notspam_wf, 5)[2]

wordcloud(names(spam_freq), spam_freq, min.freq=50,
          scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

wordcloud(names(notspam_freq), notspam_freq, min.freq=50,
          scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

spam_lemma <- udpipe(spam$CONTENT, object = udmodel)
notspam_lemma <- udpipe(notspam$CONTENT, object = udmodel)

data.frame("word" = spam_lemma[[10]]) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkgreen"), max.words = 100)

data.frame("word" = notspam_lemma[[10]]) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkgreen"), max.words = 100)

# ------------------------------------------------------------------------