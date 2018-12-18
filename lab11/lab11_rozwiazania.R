# LABORATORIUM 11 [18.12.2018]
# ------------------------------------------------------------------------

# ___ ZADANIE 1 __________________________________________________________

# Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud",
#            "biclust", "cluster", "igraph", "fpc")
# install.packages(Needed, dependencies = TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/",
#                 type = "source")

cname <- file.path("C:", "Users", "Mariusz", "Desktop",
                   "IO", "Lab Repo", "lab11", "texts")
cname
dir(cname)
library(tm)

docs <- VCorpus(DirSource(cname))   
summary(docs)

inspect(docs[1])
writeLines(as.character(docs[1]))

docs <- tm_map(docs, removePunctuation)

for (j in seq(docs)) {
    docs[[j]] <- gsub("/", " ", docs[[j]])
    docs[[j]] <- gsub("@", " ", docs[[j]])
    docs[[j]] <- gsub("\\|", " ", docs[[j]])
    docs[[j]] <- gsub("\"", " ", docs[[j]])
}

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs

length(stopwords("english"))
stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)

docs <- tm_map(docs, removeWords, c("syllogism", "tautology",
                                    "also", "can", "used"))

for (j in seq(docs)) {
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)

docs_st <- tm_map(docs, stemDocument)
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1])) # Check to see if it worked.
docs <- docs_st

docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
docs_stc <- tm_map(docs_stc, PlainTextDocument)
writeLines(as.character(docs_stc[1])) # Check to see if it worked.
docs <- docs_stc

docs <- docs_st
docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)   
dtm

tdm <- TermDocumentMatrix(docs)   
tdm

freq <- colSums(as.matrix(dtm))   
length(freq)

ord <- order(freq)

m <- as.matrix(dtm)   
dim(m)
# write.csv(m, file="DocumentTermMatrix.csv")

dtms <- removeSparseTerms(dtm, 0.2)
dtms

freq <- colSums(as.matrix(dtm))

head(table(freq), 20)

freq <- colSums(as.matrix(dtms))   
freq

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)

findFreqTerms(dtm, lowfreq=50)

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

library(ggplot2)

p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
          geom_bar(stat = "identity") + 
          theme(axis.text.x=element_text(angle=45, hjust=1))
p

findAssocs(dtm, c("computer", "life", "programming", "language"),
           corlimit=0.85)

# # install.packages("RColorBrewer")
# library(RColorBrewer)
# install.packages("wordcloud")
library(wordcloud)

set.seed(142)
wordcloud(names(freq), freq, min.freq=25)

set.seed(142)
wordcloud(names(freq), freq, max.words=100)

set.seed(142)
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)

dtmss <- removeSparseTerms(dtm, 0.15)
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

# ___ ZADANIE 2 __________________________________________________________

terms <- dtm[["dimnames"]][["Terms"]]

similar <- function(A, B) {
  return (A*B/(sum(A)*sum(B)))
}
similar(c("ha"), c("ha"))
similar(c("ha"), c("he"))

# ------------------------------------------------------------------------