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

# ----- PRZYGOTOWANIE BAZY DANYCH ----------------------------------------

library(readr)
file <- "C:/Users/Mariusz/Desktop/IO/Lab Repo/pd4/YouTube-Spam-Collection-v1/"

getFile <- function(filename) {
  return(read_csv(paste(file, filename, sep = ""), col_names = TRUE))
}

prepareDatabase <- function() {
  psy <- getFile("Youtube01-Psy.csv")
  katyperry <- getFile("Youtube02-KatyPerry.csv")
  lmfao <- getFile("Youtube03-LMFAO.csv")
  eminem <- getFile("Youtube04-Eminem.csv")
  shakira <- getFile("Youtube05-Shakira.csv")
  return(rbind(psy, katyperry, lmfao, eminem, shakira))
}

db <- prepareDatabase()

# ----- PREPROCESSING ----------------------------------------------------

library(dplyr)
db <- select(db, AUTHOR, CONTENT, CLASS)

# ----- KLASYFIKACJA TEKSTÓW ---------------------------------------------

# ...

# ----- SENTIMENT ANALYSIS -----------------------------------------------

# ...

# ------------------------------------------------------------------------