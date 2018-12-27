# LABORATORIUM 9 [27.11.2018]
# ------------------------------------------------------------------------

# ___ ZADANIE 1 __________________________________________________________

ludzie <- data.frame("wiek" = c(23, 25, 28, 22, 46, 50, 48),
                     "waga" = c(75, 67, 120, 65, 70, 68, 97),
                     "wzrost" = c(176, 180, 175, 165, 187, 180, 178),
                     "gra" = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
ludzie

w <- c(-0.46122, 0.97314, -0.39203, 0.78548, 2.10584, -0.57847)
v <- c(-0.81546, 1.03775)
b <- c(0.80109, 0.43529, -0.2368)

activate <- function(x) {
  return(1/(1+exp(-x)))
}

forwardPass <- function(wiek, waga, wzrost) {
  hidden1 <- activate((wiek * w[1]) + (waga * w[2]) + (wzrost * w[3]) + b[1])
  hidden2 <- activate((wiek * w[4]) + (waga * w[5]) + (wzrost * w[6]) + b[2])
  output <- (v[1] * hidden1) + (v[2] * hidden2) + b[3]
  return(output)
}

forwarded <- c()
for (row in 1:nrow(ludzie)) {
  tmp <- forwardPass(ludzie[row,][1], ludzie[row,][2], ludzie[row,][3])
  forwarded <- c(forwarded, tmp)
}
forwarded <- data.frame("forwarded" = as.numeric(forwarded))
forwarded

# ___ ZADANIE 2 __________________________________________________________

iris.data <- iris
norm <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}
iris.norm <- data.frame(norm(iris.data[1]), norm(iris.data[2]),
                        norm(iris.data[3]), norm(iris.data[4]),
                        iris.data[5])

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.train <- iris.norm[ind==1,1:5]
iris.test <- iris.norm[ind==2,1:5]

#install.packages("neuralnet")
#library(neuralnet)

iris.train$Setosa <- 0
iris.train$Versicolor <- 0
iris.train$Virginica <- 0

for (row in 1:nrow(iris.train)) {
  if (iris.train[row,]["Species"] == "setosa") iris.train[row,]["Setosa"] = 1
  if (iris.train[row,]["Species"] == "versicolor") iris.train[row,]["Versicolor"] = 1
  if (iris.train[row,]["Species"] == "virginica") iris.train[row,]["Virginica"] = 1
}

iris.train <- subset(iris.train, select = -c(Species))
iris.neuralnet <- neuralnet(Setosa + Versicolor + Virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                            iris.train, hidden=4)
iris.pred <- compute(iris.neuralnet, iris.test[,1:4])
plot(iris.neuralnet)

iris.pred_species <- c()
for (row in 1:nrow(iris.pred$net.result)) {
  col_number <- match(max(iris.pred$net.result[row,]), iris.pred$net.result[row,])
  if (col_number == 1) iris.pred_species <- c(iris.pred_species, "setosa")
  if (col_number == 2) iris.pred_species <- c(iris.pred_species, "versicolor")
  if (col_number == 3) iris.pred_species <- c(iris.pred_species, "virginica")
}

iris.comparison <- cbind("real" = as.character(iris.test["Species"][,1]), "predicted" = iris.pred_species)

iris.result <- c()
for (row in 1:nrow(iris.comparison)) {
  if (iris.comparison[,1][row] == iris.comparison[,2][row]) iris.result <- c(iris.result, TRUE)
  else iris.result <- c(iris.result, FALSE)
}

accuracy <- (as.numeric(table(iris.result)["TRUE"])/40)*100
accuracy

# ___ ZADANIE 3 __________________________________________________________

# ...

# ------------------------------------------------------------------------