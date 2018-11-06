# ------------------------------------------------------------------------
# funkcja fitness: 7 formuł, 3 zmienne
fitnessFunc <- function(chr)
{
  # budowanie formuły
  f <- c()
  f[1] <- (!chr[1] | chr[2]  | chr[4])      # 1-sza klauzula
  f[2] <- (!chr[2] | chr[3]  | chr[4])      # 2-ga  klauzula
  f[3] <- (chr[1]  | !chr[3] | chr[4])      # 3-cia klauzula
  f[4] <- (chr[1]  | !chr[2] | !chr[4])     # 4-ta  klauzula
  f[5] <- (chr[2]  | !chr[3] | !chr[4])     # 5-ta  klauzula
  f[6] <- (!chr[1] | chr[3]  | !chr[4])     # 6-ta  klauzula
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
# ------------------------------------------------------------------------
# algorytm genetyczny z paczki "genalg"

# różne szanse mutacji
trisatGenAlg00mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.0, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg01mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.1, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg02mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.2, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg03mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.3, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg04mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.4, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg05mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.5, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg06mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.6, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg07mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.7, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg08mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.8, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg09mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 0.9, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg10mut <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                              mutationChance = 1.0, elitism = T, evalFunc = fitnessFunc))

czasy <- c(trisatGenAlg00mut[["elapsed"]][1],
           trisatGenAlg01mut[["elapsed"]][1],
           trisatGenAlg02mut[["elapsed"]][1],
           trisatGenAlg03mut[["elapsed"]][1],
           trisatGenAlg04mut[["elapsed"]][1],
           trisatGenAlg05mut[["elapsed"]][1],
           trisatGenAlg06mut[["elapsed"]][1],
           trisatGenAlg07mut[["elapsed"]][1],
           trisatGenAlg08mut[["elapsed"]][1],
           trisatGenAlg09mut[["elapsed"]][1],
           trisatGenAlg10mut[["elapsed"]][1])
rozmiary <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

plot(rozmiary, czasy, type="o",
     main="Czas działania Alg. Gen. - szansa mutacji",
     xlab="Szansa mutacji", xlim=c(0, 1),
     ylab="Czas trwania obliczeń", ylim=c(1.2,2.7),
     pch=16, col="green", lab = c(11,16,0))
lines(rozmiary, czasy, type="p", pch=16, col="darkgreen")
text(rozmiary, czasy+0.05, labels=round(czasy,2), cex= 0.75)
# ------------------------------------------------------------------------
# różna wielkość populacji
trisatGenAlg01pop <- system.time(rbga.bin(size = 4, popSize = 50, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg02pop <- system.time(rbga.bin(size = 4, popSize = 100, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg03pop <- system.time(rbga.bin(size = 4, popSize = 150, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg04pop <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg05pop <- system.time(rbga.bin(size = 4, popSize = 250, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg06pop <- system.time(rbga.bin(size = 4, popSize = 300, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg07pop <- system.time(rbga.bin(size = 4, popSize = 350, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg08pop <- system.time(rbga.bin(size = 4, popSize = 400, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg09pop <- system.time(rbga.bin(size = 4, popSize = 450, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))
trisatGenAlg10pop <- system.time(rbga.bin(size = 4, popSize = 500, iters = 100,
                                          mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc))


czasy <- c(trisatGenAlg01pop[["elapsed"]][1],
           trisatGenAlg02pop[["elapsed"]][1],
           trisatGenAlg03pop[["elapsed"]][1],
           trisatGenAlg04pop[["elapsed"]][1],
           trisatGenAlg05pop[["elapsed"]][1],
           trisatGenAlg06pop[["elapsed"]][1],
           trisatGenAlg07pop[["elapsed"]][1],
           trisatGenAlg08pop[["elapsed"]][1],
           trisatGenAlg09pop[["elapsed"]][1],
           trisatGenAlg10pop[["elapsed"]][1])
rozmiary <- c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500)

plot(rozmiary, czasy, type="o",
     main="Czas działania Alg. Gen. - wielkość populacji",
     xlab="Wielkość populacji", xlim=c(50, 500),
     ylab="Czas trwania obliczeń", ylim=c(0,7),
     pch=16, col="green", lab = c(11,16,0))
lines(rozmiary, czasy, type="p", pch=16, col="darkgreen")
text(rozmiary, czasy+0.25, labels=round(czasy,2), cex= 0.75)
# ------------------------------------------------------------------------
