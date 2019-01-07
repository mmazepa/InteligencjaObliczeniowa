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

dane <- data.frame(dane[1:9], dane[10:16], dane[29:37])

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

# ----- NEURALNET - EWALUACJA KLASYFIKATORA ------------------------------

outputNames <- names(dane[10:16])
inputNames <- names(dane[-10:-16])

hiddenAmount <- 10
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

roundOnly <- function(x) {
  return(toString(round(x, 3)))
}

roundOnly(bias1)
roundOnly(bias2)
roundOnly(t(weights1))
roundOnly(t(weights2))

# ------------------------------------------------------------------------
# ----- GRAFICZNE PRZEDSTAWIENIE FINALNYCH WAG I BIASU -------------------
# ------------------------------------------------------------------------

w1 <- c(-3.2, -4.3, -6.9, 1.5, 3.9, -8.2, 3.5, -5.3, 1.2, 8.4, -2.5, -11.2, 10.3, -12.3, 15.7, 15.9, 13.9, 7.2, 15.5, 14.2, 15.0, 3.3, 10.8, 2.2, 10.4, 6.0, -15.5, -12.4, 0.5, -4.4, -6.4, 11.5, 10.1, -0.0, -15.4, -3.6, -0.5, -3.5, 1.4, 0.1, -2.4, -13.9, 4.6, -4.6, 9.8, -8.5, 6.1, 7.1, -7.5, 11.2, 7.0, -0.4, -5.8, -14.6, 6.5, -14.6, 10.1, 5.4, 10.3, -10.1, -6.7, 12.1, 7.4, 9.1, -6.4, 9.8, 13.5, 10.8, -14.4, -6.0, 2.3, 13.1, 12.2, -14.8, -15.1, -8.1, -1.6, -7.8, -11.1, 7.8, -10.5, 8.3, 3.9, 1.7, -10.3, -15.2, -4.2, 0.3, 15.9, 3.8, -7.0, 0.1, -8.5, 7.7, 9.3, 2.7, 3.7, 14.4, -8.9, -8.7, -6.3, 1.5, -11.5, 8.4, -13.8, -15.7, -8.5, 4.4, 5.5, 12.3, -11.8, 7.2, -1.5, 7.5, 9.2, 13.2, -15.6, -5.4, 13.1, 3.4, 6.5, -0.7, -9.9, -10.8, 2.8, 13.4, -8.6, -5.3, -0.8, -3.5, -4.1, -11.8, -10.4, -10.8, -4.6, -3.6, 10.5, 12.0, -1.1, 10.5, 10.2, -12.6, 3.6, 2.1, 2.9, 6.7, -13.5, 10.0, 11.2, 6.9, -9.3, 11.5, 9.7, 15.3, 6.5, 0.9, 8.1, -15.5, 9.2, 10.8, -0.8, -5.9, -7.4, 5.8, -3.7, 14.1, 8.6, -5.4, -4.3, -9.6, 13.5, 12.3, 7.6, 14.2, 10.3, 13.6, -8.7, -8.9, -14.8, -7.2)
b1 <- c(0.6, 1.2, -3.0, -0.2, 0.4, 3.5, 3.8, -2.3, -2.0, -1.2)

w2 <- c(-5.7, -9.2, -0.7, -2.2, -12.5, 9.0, -15.1, -6.1, -5.3, 11.6, -1.4, -12.6, 0.5, -11.9, 10.9, 6.6, -1.9, -2.2, -9.1, -14.1, 10.2, -11.3, 0.4, -6.5, 2.7, -12.6, 9.3, -11.6, 13.3, -0.4, -5.3, 3.4, 5.1, -15.6, 7.1, -5.7, 6.4, 0.4, 0.0, -9.5, -11.5, 1.4, 8.9, -12.4, 5.5, 7.9, 13.7, -8.5, 4.8, 7.0, -12.8, 7.6, 0.6, -8.5, -11.7, -8.2, 2.0, -4.2, -12.3, 1.2, -13.1, 14.7, 7.4, -1.9, 2.8, 14.0, -4.2, -14.8, 6.3, 6.1)
b2 = c(-0.6, -2.1, 0.0, 2.3, 1.3, 1.1, -3.3)

weights1 <- matrix(matrix(c(b1,w1)), nrow=10, ncol=19, byrow=FALSE)
dimnames(weights1) = list(c(1:10), c(1:19))

weights2 <- matrix(matrix(c(b2,w2)), nrow=7, ncol=11, byrow=FALSE)
dimnames(weights2) = list(c(1:7), c(1:11))

bias1 <- b1
bias2 <- b2

dane.neuralnet[["weights"]][[1]][[1]][1:(inputAmount+1),] <- t(weights1)
dane.neuralnet[["weights"]][[1]][[2]][1:(hiddenAmount+1),] <- t(weights2)

plot(dane.neuralnet)

# ------------------------------------------------------------------------