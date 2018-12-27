# LABORATORIUM 5 [30.10.2018]
# -----------------------------------------------------------------------

# ZADANIE 1

# a)

myPredictRow <- function(sl,sw,pl,pw) {
    if (sl >= 4 && sw > 3 && pl < 2 && pw < 1) {
      return("setosa")
    } else {
      if (sl >= 4 && sw < 3 && pl >= 3 && pw >= 1) {
        return("versicolor")
      } else {
        return("virginica")
      }
    }
}

# b)

db <- iris
count <- 0

myPredict <- function() {
  for(i in 1:150) {
    x <- db[i,]
    if (x["Species"] == myPredictRow(x[1], x[2], x[3], x[4]))
      count <- count + 1
  }
  return((count/150)*100)
}
myPredict()

# c) Odp: Mój klasyfikator działa średnio (>60%).

# ZADANIE 2

# a)

#install.packages("party")
#library("party")

# b)

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.training <- iris[ind==1,1:5]
iris.test <- iris[ind==2,1:5]

# c)

iris.ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris.training)

# d)

print(iris.ctree)

plot(iris.ctree)
plot(iris.ctree, type="simple")

# e)

iris.test$Species
predict(iris.ctree, iris.test[,1:4])

# f)

predicted <- predict(iris.ctree, iris.test[,1:4])
real <- iris.test[,5]
table(predicted,real)

# g)

# ???
# -----------------------------------------------------------------------