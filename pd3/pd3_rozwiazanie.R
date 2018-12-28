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

dane <- dane[1:5000,]

# ----- POTRZEBNE PACZKI - NEURALNET -------------------------------------

#install.packages("neuralnet")
library(neuralnet)

# ----- NORMALIZACJA DANYCH ----------------------------------------------

norm <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}

dane.norm2 <- dane.norm <- data.frame(norm(dane[1:50]))

# ----- PODZIA£ NA ZBIÓR TRENINGOWY I TESTOWY ----------------------------

set.seed(1234)
ind <- sample(2, nrow(dane.norm), replace=TRUE, prob=c(0.67, 0.33))
dane.train <- dane.norm[ind==1,1:length(dane)]
dane.test <- dane.norm[ind==2,1:length(dane)]

# ----- POZBYCIE SIÊ WARTOŒCI NaN (Not a Number) -------------------------

is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}
dane.train[is.nan(dane.train)] <- 0
dane.test[is.nan(dane.test)] <- 0

# ----- NEURALNET - EWALUACJA KLASYFIKATORA ------------------------------

outputNames <- names(dane[10:16]);
inputNames <- names(c(dane[1:9], dane[17:length(dane)]))

hiddenAmount <- 20
outputAmount <- length(outputNames)
inputAmount <- length(inputNames)

formula <- paste(paste(outputNames, collapse=" + "),
                 paste(inputNames, collapse=" + "),
                 sep=" ~ ")

dane.neuralnet <- neuralnet(formula, dane.train, hidden=hiddenAmount)
plot(dane.neuralnet)

# ----- WEIGHTS & BIAS: WYCI¥GNIÊCIE -------------------------------------

bias1 <- dane.neuralnet[["weights"]][[1]][[1]][1,]
bias2 <- dane.neuralnet[["weights"]][[1]][[2]][1,]

weights1 <- dane.neuralnet[["weights"]][[1]][[1]][2:(inputAmount+1),]
weights2 <- dane.neuralnet[["weights"]][[1]][[2]][2:(hiddenAmount+1),]

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
# toString(weights1)
# toString(weights2)

# ------------------------------------------------------------------------