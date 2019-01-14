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

psy <- read_csv(paste(file, "Youtube01-Psy.csv", sep = ""), col_names = TRUE)
katyperry <- read_csv(paste(file, "Youtube02-KatyPerry.csv", sep = ""), col_names = TRUE)
lmfao <- read_csv(paste(file, "Youtube03-LMFAO.csv", sep = ""), col_names = TRUE)
eminem <- read_csv(paste(file, "Youtube04-Eminem.csv", sep = ""), col_names = TRUE)
shakira <- read_csv(paste(file, "Youtube05-Shakira.csv", sep = ""), col_names = TRUE)

# ----- PREPROCESSING ----------------------------------------------------

# ...

# ----- KLASYFIKACJA TEKSTÓW ---------------------------------------------

# ...

# ----- SENTIMENT ANALYSIS -----------------------------------------------

# ...

# ------------------------------------------------------------------------