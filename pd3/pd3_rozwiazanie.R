# ------------------------------------------------------------------------
# PRACA DOMOWA 3
# ------------------------------------------------------------------------
# IMIÊ I NAZWISKO:   Mariusz Mazepa
# NUMER INDEKSU:     235371
# PRZEDMIOT:         Inteligencja Obliczeniowa
# GRUPA ÆWICZENIOWA: 3
# ZAGADNIENIE:       Sieci neuronowe steruj¹ce czo³gami
# ------------------------------------------------------------------------

# ----- PRZYGOTOWANIE BAZY DANYCH ----------------------------------------

library(readr)
file <- "C:/Users/Mariusz/Desktop/IO/Lab Repo/pd3/dane.csv"
dane <- read_csv(file, col_names = TRUE)

# ----- POZBYCIE SIÊ NIEPOTRZEBNYCH KOLUMN -------------------------------

dane <- data.frame(dane[1:9], dane[10:16], dane[29:30])

# ----- POTRZEBNE PACZKI - NEURALNET -------------------------------------

#install.packages("neuralnet")
library(neuralnet)

# ----- NORMALIZACJA DANYCH ----------------------------------------------

norm <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

dane.norm <- norm(dane)

# ----- PODZIA£ NA ZBIÓR TRENINGOWY I TESTOWY ----------------------------

set.seed(1234)
ind <- sample(2, nrow(dane.norm), replace=TRUE, prob=c(0.67, 0.33))
dane.train <- dane.norm[ind==1, 1:length(dane)]
dane.test <- dane.norm[ind==2, 1:length(dane)]

# ----- POZBYCIE SIÊ WARTOŒCI NaN (Not a Number) -------------------------

is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}
dane.train[is.nan(dane.train)] <- 0
dane.test[is.nan(dane.test)] <- 0

# ----- NEURALNET - EWALUACJA KLASYFIKATORA ------------------------------

outputNames <- names(dane[10:16])
inputNames <- names(dane[-10:-16])

hiddenAmount <- 5
outputAmount <- length(outputNames)
inputAmount <- length(inputNames)

formula <- paste(paste(outputNames, collapse=" + "),
                 paste(inputNames, collapse=" + "),
                 sep=" ~ ")

dane.neuralnet <- neuralnet(formula, dane.train, hidden=hiddenAmount,
                            threshold = 0.1, stepmax = 1e+06)
plot(dane.neuralnet)

# ----- WEIGHTS & BIAS: WYCI¥GNIÊCIE -------------------------------------

bias1 <- dane.neuralnet[["weights"]][[1]][[1]][1,]
bias2 <- dane.neuralnet[["weights"]][[1]][[2]][1,]

weights1 <- dane.neuralnet[["weights"]][[1]][[1]][2:(inputAmount+1),]
weights2 <- dane.neuralnet[["weights"]][[1]][[2]][2:(hiddenAmount+1),]

bias1 <- round(bias1, 3)
bias2 <- round(bias2, 3)
weights1 <- round(weights1, 3)
weights2 <- round(weights2, 3)

# bias1
# bias2
# weights1
# weights2

# ----- WEIGHTS & BIAS: SPRAWDZENIE --------------------------------------

length(bias1) == hiddenAmount
length(bias2) == outputAmount
length(weights1) == inputAmount * hiddenAmount
length(weights2) == hiddenAmount * outputAmount

# ----- WEIGHTS & BIAS: STRINGI DO KOPIOWANIA ----------------------------

# toString(bias1)
# toString(bias2)
# toString(t(weights1))
# toString(t(weights2))

# ------------------------------------------------------------------------