# LABORATORIUM 1 [02.10.2018]
# -----------------------------------------------------------------------

# ZADANIE 1

45*678

x <- c(7, 4, 2, 0, 9)
y <- c(2, 1, 5, 3, 3)
x+y
x*y

m1 <- matrix(c(0, 2, 1, 1, 6, 4, 5, 0, 3), nrow=3, ncol=3, byrow = TRUE)
m2 <- matrix(c(9, 8, 7, 1, 2, 7, 4, 9, 2), nrow=3, ncol=3, byrow = TRUE)
m1 %*% m2

suma <- function(a,b)
{
  return(a+b)
}
suma(5,7)
suma(11,4)

save.image()
load(".RData")

# ZADANIE 2

getwd()
setwd("/home/LABPK/mmazepa/Pulpit/io/lab1")
#setwd("/home/mariusz/Pulpit/io/lab1")

plik <- read.csv("osoby.csv", stringsAsFactors = FALSE)
plik
plik["imie"]
subset(plik, plec == "k")

mezczyzni = subset(plik, plec == "m" & wiek > 50)
mezczyzni
write.csv(mezczyzni, "osoby2.csv")

# ZADANIE 3

plik["wyplata"] <- c(round(runif(1:7, 2000, 5000), digits=2))
plik

nowy <- data.frame(imie="Jan", nazwisko="Kowalski", "m", 37, round(runif(1, 2000, 5000), digits=2))
plik <- rbind(plik, nowy)
#jakiś błąd w rbind(...) - zastanowić się nad nim!!!

pensje <- as.numeric(plik$wyplata)
srednia <- mean(pensje)
odchylenie <- sd(pensje)
minimum <- min(pensje)
maximum <- max(pensje)

stand <- function(x)
{
  return((x-srednia)/odchylenie)
}

plik["standard"] <- stand(pensje)
plik

stand_pensje <- as.numeric(plik$standard)
mean(stand_pensje)
sd(stand_pensje)

norm <- function(x)
{
  return((x-minimum)/(maximum-minimum))
}

plik["normal"] <- norm(pensje)
plik

min(plik$normal)
max(plik$normal)

plik["wiek"] <- sapply(plik["wiek"], as.numeric)

as.numeric(gsub('.*:', '', summary(plik["wiek"])[4]))

# ZADANIE 4

normalize <- function(input)
{
  return(norm(input))
}

standarize <- function(input)
{
  return(stand(input))
}

wektor1 <- c(1, 2, 3)
wektor2 <- c(5, 7, 6, 4, 10)

normalize(wektor1)
normalize(wektor2)
standarize(wektor1)
standarize(wektor2)

# -----------------------------------------------------------------------