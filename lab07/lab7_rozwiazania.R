# LABORATORIUM 7 [13.11.2018]
# ------------------------------------------------------------------------

# ___ ZADANIE 1 __________________________________________________________

rozowy <- c(1,6)
zielony <- c(8,1)

x1 <- c(1,3)
x2 <- c(2,5)
x3 <- c(3,2)
x4 <- c(5,2)
x5 <- c(5,6)
x6 <- c(7,4)
x7 <- c(9,5)

rozowe <- c()
zielone <- c()

algKsrednich <- function(x)
{
  odl1 <- sqrt((rozowy[2]-rozowy[1])^2 + (x[2]-x[1])^2)
  odl2 <- sqrt((zielony[2]-zielony[1])^2 + (x[2]-x[1])^2)
  
  print(odl1)
  print(odl2)
  if (odl1 >= odl2)
  {
    rozowe <- c(rozowe, x)
    rozowy <- c((rozowy[1]+x[1])/2, (rozowy[2]+x[2])/2)
    return(data.frame(kolor = "ró¿owy", punkt = rozowy, lista = rozowe))
  } else {
    zielone <- c(zielone, x)
    zielony <- c((zielony[1]+x[1])/2, (zielony[2]+x[2])/2)
    return(data.frame(kolor = "zielony", punkt = zielony, lista = zielone))
  }
}

algKsrednich(x1)
algKsrednich(x2)
algKsrednich(x3)
algKsrednich(x4)
algKsrednich(x5)
algKsrednich(x6)
algKsrednich(x7)

rozowe
zielone

# ___ ZADANIE 2 __________________________________________________________

#install.packages("editrules")
#library(editrules)

iris.log <- log(iris[,1:4])
iris.scale <- scale(iris.log, center=TRUE)
iris.pca <- prcomp(iris.scale)
iris.final <- predict(iris.pca)
iris.final <- iris.final[,1:2]

iris.kmeans <- kmeans(iris.final, 3)

plot(iris.final, col = iris.kmeans[["cluster"]], main="Algorytm grupowania")
points(iris.kmeans[["centers"]], col = 1:3, pch = 16, cex=1.5)

# ------------------------------------------------------------------------