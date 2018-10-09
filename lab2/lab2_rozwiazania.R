# LABORATORIUM 2 [09.10.2018]
# -----------------------------------------------------------------------

# NOTATKI

# Algorytmy genetyczne <- inspirowane naturą
# rozwiązanie (kandydat)

# ZADANIE 1

# a)

#install.packages("genalg")
#library(genalg)

# b)

plecakdb <- data.frame(
  przedmiot = c("zegar", "obraz-pejzaż", "obraz-portret", "radio", "laptop",
                "lampka nocna", "srebrne sztućce", "porcelana", "figura z brązu",
                "skórzana torebka", "odkurzacz"),
  wartosc = c(100, 300, 200, 40, 500, 70, 100, 250, 300,280,300),
  waga = c(7, 7, 6, 2, 5, 6, 1, 3, 10, 3, 15))

plecaklimit <- 25

# c)

chromosome = c(0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1)
plecakdb[chromosome == 1, ]
cat(chromosome %*% plecakdb$wartosc)

# d)

fitnessFunc <- function(chr)
{
  calkowita_wartosc_chr <- chr %*% plecakdb$wartosc
  calkowita_waga_chr <- chr %*% plecakdb$waga
  if (calkowita_waga_chr > plecaklimit)
    return(0)
  else
    return(-calkowita_wartosc_chr)
}

# e)

plecakGenAlg <- rbga.bin(size = 11, popSize = 200, iters = 100,
                mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc)
summary(plecakGenAlg, echo=TRUE)

# f)

chromosome2 = c(0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0)
plecakdb[chromosome2 == 1, ]
cat(chromosome2 %*% plecakdb$wartosc)

# g)

plecakGenAlg

# ZADANIE 2

# Problem LABIRYNT

labirynt <- matrix(
  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), nrow=12, ncol=12
)

# -----------------------------------------------------------------------