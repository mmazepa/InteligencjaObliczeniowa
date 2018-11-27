# LABORATORIUM 9 [27.11.2018]
# ------------------------------------------------------------------------

# ___ ZADANIE 1 __________________________________________________________

ludzie <- data.frame("wiek" = c(23, 25, 28, 22, 46, 50, 48),
                     "waga" = c(75, 67, 120, 65, 70, 68, 97),
                     "wzrost" = c(176, 180, 175, 165, 187, 180, 178),
                     "gra" = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))

w1 <- -0.46122
w2 <- 0.97314
w3 <- -0.39203
w4 <- 0.78548
w5 <- 2.10584
w6 <- -0.57847
b1 <- 0.80109
b2 <- 0.43529
b3 <- -0.2368
v1 <- -0.81546
v2 <- 1.03775

forwardPass <- function(wiek, waga, wzrost) {
  hidden1 <- wiek * w1 + wzrost * w2 + waga * w3 + b1
  hidden1 <- 1/(1+exp(-hidden1))
  hidden2 <- wiek * w4 + wzrost * w5 + waga * w6 + b2
  hidden2 <- 1/(1+exp(-hidden2))
  output <- v1 * hidden1 + v2 * hidden2 + b3
  return(output)
}

forwardPass(23, 75, 176)
forwardPass(50, 68, 180)

# ___ ZADANIE 2 __________________________________________________________

iris.data <- iris
norm <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}
iris.data.norm <- data.frame(norm(iris.data[1]), norm(iris.data[2]),
                             norm(iris.data[3]), norm(iris.data[4]),
                             iris.data[5])

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.norm.training <- iris.data.norm[ind==1,1:5]
iris.norm.test <- iris.data.norm[ind==2,1:5]

#install.packages("neuralnet")
#library(neuralnet)

#neuralnet(iris.norm.training)

iris.norm.training$Setosa <- 0
iris.norm.training$Versicolor <- 0
iris.norm.training$Virginica <- 0

for (row in 1:nrow(iris.norm.training)) {
  if (iris.norm.training[row,]["Species"] == "setosa") iris.norm.training[row,]["Setosa"] = 1
  if (iris.norm.training[row,]["Species"] == "versicolor") iris.norm.training[row,]["Versicolor"] = 1
  if (iris.norm.training[row,]["Species"] == "virginica") iris.norm.training[row,]["Virginica"] = 1
}

iris.norm.training <- subset(iris.norm.training, select = -c(Species))
iris.neuralnet <- neuralnet(Setosa + Versicolor + Virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris.norm.training)
compute(iris.norm.test)

# ___ ZADANIE 3 __________________________________________________________

# ...

# ------------------------------------------------------------------------