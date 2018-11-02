# LABORATORIUM 4 [23.10.2018]
# -----------------------------------------------------------------------

# ZADANIE 1

# a)

setwd("/home/mariusz/Pulpit/io/lab4")
dirty.iris <- read.csv("dirty_iris.csv", header=TRUE, sep=",")

nrow(subset(dirty.iris, is.finite(Sepal.Length) & is.finite(Sepal.Width)
       & is.finite(Petal.Length) & is.finite(Petal.Width)))

# b) & c)

#install.packages("editrules")
#library(editrules)

species = c("setosa", "versicolor", "virginica")

E <- editset(c("Sepal.Length <= 30",
               "Species %in% species",
               "Sepal.Length > 0",
               "Sepal.Width > 0",
               "Petal.Length > 0",
               "Petal.Width > 0",
               "Petal.Length >= 2*Petal.Width",
               "Sepal.Length > Petal.Length",
               "Sepal.Width > Petal.Width"))
E

ve <- violatedEdits(E, dirty.iris)
ve

summary(ve)
#plot(ve)

# d)

nrow(violatedEdits(E, dirty.iris))

# ZADANIE 2

#install.packages("deducorrect")
#library(deducorrect)

correctWithRules()

# -----------------------------------------------------------------------

