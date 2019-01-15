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

    comments <- tm_map(comments, stripWhitespace)
  comments <- tm_map(comments, PlainTextDocument)  
}

comments <- preprocessing()

# ----- TWORZENIE DANYCH DO ANALIZY --------------------------------------

dtm <- DocumentTermMatrix(comments)
dtm

tdm <- TermDocumentMatrix(comments)
tdm

# ----- EKSPLORACJA DANYCH -----------------------------------------------

findFreqTerms(dtm, lowfreq=50)

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

freq <- colSums(as.matrix(dtm))
head(table(freq), 20)
tail(table(freq), 20)

dtms <- removeSparseTerms(dtm, 0.2)
freq <- colSums(as.matrix(dtms))
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf <- data.frame(word=names(freq), freq=freq)   
head(wf, 10)

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

# ----- WORD CLOUDS! -----------------------------------------------------

#install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

set.seed(142)
wordcloud(names(freq), freq, min.freq=50)
wordcloud(names(freq), freq, max.words=100)

wordcloud(names(freq), freq, min.freq=50,
          scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

# ----- KLASTROWANIE WED£UG PODOBIEÑSTWA TEKSTÓW -------------------------

dtmss <- removeSparseTerms(dtm, 0.5)
dtmss

library(cluster)
d <- dist(t(dtmss), method="euclidian")
fit <- hclust(d=d, method="complete")
fit

plot(fit, hang=-1)

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)
rect.hclust(fit, k=6, border="red")

library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

# ----- KLASYFIKACJA TEKSTÓW ---------------------------------------------

# ...

# ----- SENTIMENT ANALYSIS -----------------------------------------------

# ...

# ------------------------------------------------------------------------