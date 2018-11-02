# PRACA DOMOWA 1
# ------------------------------------------------------------------------
# IMIĘ I NAZWISKO :  Mariusz Mazepa
# NUMER INDEKSU:     235371
# PRZEDMIOT:         Inteligencja Obliczeniowa
# GRUPA ĆWICZENIOWA: 3
# WYBRANY PROBLEM :  3-SAT
# ------------------------------------------------------------------------

#install.packages("genalg")                  # instalacja paczki "genalg"
#library(genalg)                             # wybranie paczki "genalg"

fitnessFunc <- function(chr)
{
  # budowanie formuły
  f <- c()
  f[1] <- (!chr[1] | chr[2]  | chr[4])      # 1-sza klauzula
  f[2] <- (!chr[2] | chr[3]  | chr[4])      # 2-ga  klauzula
  f[3] <- (chr[1]  | !chr[3] | chr[4])      # 3-cia klauzula
  f[4] <- (chr[1]  | !chr[2] | !chr[4])     # 4-ta  klauzula
  f[5] <- (chr[2]  | !chr[3] | !chr[4])     # 5-ta  klauzula
  f[6] <- (!chr[2] | chr[3]  | !chr[4])     # 6-ta  klauzula
  f[7] <- (chr[1]  | chr[2]  | chr[3])      # 7-ma  klauzula
  
  # iterowanie po formule, zliczanie jedynek
  ilosc_jedynek <- 0;
  for (klauzula in f)
  {
    if (klauzula == TRUE) ilosc_jedynek = ilosc_jedynek + 1
  }
  # ocena chromosomu
  return(-ilosc_jedynek)
}

# algorytm genetyczny z paczki "genalg"
trisatGenAlg <- rbga.bin(size = 4,
                         popSize = 200,
                         iters = 100,
                         mutationChance = 0.05,
                         elitism = T,
                         evalFunc = fitnessFunc)
summary(trisatGenAlg, echo=TRUE)

# sprawdzenie
bestSolution <- trisatGenAlg$population[which.min(trisatGenAlg$evaluations),]
if (fitnessFunc(bestSolution) == -7) print("sukces") else print("porażka")

# preparowanie wykresu
chartData <- data.frame(srednia = -trisatGenAlg$mean,
                        maksymalne = -trisatGenAlg$best)

plot(chartData$maksymalne,
     type="l",
     main="Działanie Alg. Genetycznego",
     xlab="pokolenie",
     ylab="fitness (ocena)",
     col="red")

lines(chartData$srednia, col="blue")

legend("bottomright",
       c("srednia", "maksymalnie"),
       lty=c(1,1),
       col=c("blue", "red"),
       title = "legenda")
# ------------------------------------------------------------------------