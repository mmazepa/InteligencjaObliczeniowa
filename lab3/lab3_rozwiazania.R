# LABORATORIUM 3
# -----------------------------------------------------------------------

# ZADANIE 1

# a)

#install.packages("genalg")
#library(genalg)

duzyProblemPlecakowy <- data.frame(wartosc = sample(10:100,30),
                                   waga = sample(10:100,30))

duzyLimit <- 600

fitnessFunc2 <- function(chr)
{
  calkowita_wartosc_chr <- chr %*% duzyProblemPlecakowy$wartosc
  calkowita_waga_chr <- chr %*% duzyProblemPlecakowy$waga
  if (calkowita_waga_chr > duzyLimit)
    return(0)
  else
    return(-calkowita_wartosc_chr)
}

duzyPlecakGenAlg <- rbga.bin(size = 30, popSize = 200, iters = 50,
           mutationChance = 0.03, elitism = T, evalFunc = fitnessFunc2)

# b)

chartData <- data.frame(srednia = -duzyPlecakGenAlg$mean,
                        maksymalne = -duzyPlecakGenAlg$best)

# c)

plot(chartData$maksymalne, type="l",
     main="Działanie Alg. Genetycznego",
     xlab="pokolenie", ylab="fitness (ocena)",
     col="red")

# d)

lines(chartData$srednia, col="blue")

# e)

legend("bottomright", c("srednia", "maksymalnie"),
       lty=c(1,1), col=c("blue", "red"), title = "legenda")

# ZADANIE 2

duzyProblemPlecakowy1 <- data.frame(wartosc = sample(10:300,60),
                                    waga = sample(10:300,60))
duzyProblemPlecakowy2 <- data.frame(wartosc = sample(10:300,120),
                                    waga = sample(10:300,120))
duzyProblemPlecakowy3 <- data.frame(wartosc = sample(10:300,200),
                                    waga = sample(10:300,200))

fitnessFunc3 <- function(chr)
{
  calkowita_wartosc_chr <- chr %*% duzyProblemPlecakowy1$wartosc
  calkowita_waga_chr <- chr %*% duzyProblemPlecakowy1$waga
  if (calkowita_waga_chr > duzyLimit)
    return(0)
  else
    return(-calkowita_wartosc_chr)
}

fitnessFunc4 <- function(chr)
{
  calkowita_wartosc_chr <- chr %*% duzyProblemPlecakowy2$wartosc
  calkowita_waga_chr <- chr %*% duzyProblemPlecakowy2$waga
  if (calkowita_waga_chr > duzyLimit)
    return(0)
  else
    return(-calkowita_wartosc_chr)
}

fitnessFunc5 <- function(chr)
{
  calkowita_wartosc_chr <- chr %*% duzyProblemPlecakowy3$wartosc
  calkowita_waga_chr <- chr %*% duzyProblemPlecakowy3$waga
  if (calkowita_waga_chr > duzyLimit)
    return(0)
  else
    return(-calkowita_wartosc_chr)
}

duzyPlecakGenAlg1 <- system.time(rbga.bin(size = 60, popSize = 200, iters = 100,
                             mutationChance = 0.03, elitism = T, evalFunc = fitnessFunc3))
duzyPlecakGenAlg2 <- system.time(rbga.bin(size = 120, popSize = 200, iters = 100,
                             mutationChance = 0.03, elitism = T, evalFunc = fitnessFunc4))
duzyPlecakGenAlg3 <- system.time(rbga.bin(size = 200, popSize = 200, iters = 100,
                             mutationChance = 0.03, elitism = T, evalFunc = fitnessFunc5))

czasy <- c(duzyPlecakGenAlg1[["elapsed"]][1],
           duzyPlecakGenAlg2[["elapsed"]][1],
           duzyPlecakGenAlg3[["elapsed"]][1])
rozmiary <- c(60, 120, 200)

plot(rozmiary, czasy, type="o", main="Działanie Alg. Genetycznego 2",
     xlab="Długość chromosomu", ylab="Czas trwania obliczeń",
     pch=16, col="green")
lines(rozmiary, czasy, type="p", pch=16, col="darkgreen")
# -----------------------------------------------------------------------