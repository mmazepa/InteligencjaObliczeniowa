# LABORATORIUM 6 [06.11.2018]
# ------------------------------------------------------------------------

# ___ ZADANIE 1 __________________________________________________________

iris.data <- iris
norm <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}
iris.data.norm <- c(norm(iris.data[1]), norm(iris.data[2]),
                    norm(iris.data[3]), norm(iris.data[4]))

#install.packages("party")
#library("party")

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.norm.training <- iris[ind==1,1:5]
iris.norm.test <- iris[ind==2,1:5]

#install.packages("class")
#library("class")

knn.3 <- knn(iris.norm.training[,1:4], iris.norm.test[,1:4],
             cl=iris.norm.training[,5], k = 3, prob=FALSE)

predicted <- knn.3
real <- iris.norm.test[,5]
conf.matrix <-  table(predicted,real)
accuracy <- sum(diag(conf.matrix))/sum(conf.matrix)

# ___ ZADANIE 2 __________________________________________________________

# age   | income | student | credit.rating | buys
# -----------------------------------------------
# 31-40 | high   | no      | fair          | yes
# >40   | medium | no      | fair          | yes
# >40   | high   | yes     | excellent     | yes
# >40   | low    | yes     | excellent     | no
# 31-40 | low    | no      | excellent     | yes
# <=30  | medium | no      | fair          | no
# <=30  | low    | yes     | fair          | no
# >40   | medium | no      | excellent     | yes
# -----------------------------------------------
# >40   | low    | no      | fair          | ???

Pbuys.yes = 5/8
Pbuys.no = 3/8

PageGt40buys.yes = 3/5
PageLt40buys.no = 1/3
PincomeMediumbuys.yes = 2/5
PincomeMediumbuys.no = 1/3
PstudentNobuys.yes = 1/5 
PstudentNobuys.no = 2/3
PcreditrateExcellentbuys.yes = 3/5
PcreditrateExcellentbuys.no = 1/3

PXbuys.yes = (3/5) * (2/5) * (1/5) * (3/5)
PXbuys.no = (1/3) * (1/3) * (2/3) * (1/3)

Pbuys.yesX = PXbuys.yes * Pbuys.yes
Pbuys.noX = PXbuys.no * Pbuys.no

Pbuys.yesX > Pbuys.noX
print("Nasz rekord Y przyjmuje klasê 'yes'!")

# ------------------------------------------------------------------------