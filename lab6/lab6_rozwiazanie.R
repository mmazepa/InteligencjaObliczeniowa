# LABORATORIUM 6 [06.11.2018]
# ------------------------------------------------------------------------

# ___ ZADANIE 1 __________________________________________________________

iris.data <- iris
norm <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}
iris.data.norm <- data.frame(norm(iris.data[1]), norm(iris.data[2]),
                    norm(iris.data[3]), norm(iris.data[4]), iris.data[5])

#install.packages("party")
#library("party")

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.norm.training <- iris.data.norm[ind==1,1:5]
iris.norm.test <- iris.data.norm[ind==2,1:5]

#install.packages("class")
#library("class")

knn.3 <- knn(iris.norm.training[,1:4], iris.norm.test[,1:4],
             cl=iris.norm.training[,5], k = 3, prob=FALSE)

predicted <- knn.3
real <- iris.norm.test[,5]
conf.matrix <- table(predicted,real)
accuracy <- sum(diag(conf.matrix))/sum(conf.matrix)

conf.matrix
accuracy

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

P.buys.yes = 5/8
P.buys.no = 3/8

P.ageGt40buys.yes = 3/5
P.ageLt40buys.no = 1/3
P.incomeLowbuys.yes = 2/5
P.incomeLowbuys.no = 1/3
P.studentNobuys.yes = 1/5 
P.studentNobuys.no = 2/3
P.creditrateFairbuys.yes = 3/5
P.creditrateFairbuys.no = 1/3

P.Xbuys.yes = (3/5) * (2/5) * (1/5) * (3/5)
P.Xbuys.no = (1/3) * (1/3) * (2/3) * (1/3)

P.buys.yesX = P.Xbuys.yes * P.buys.yes
P.buys.noX = P.Xbuys.no * P.buys.no

if (P.buys.yesX > P.buys.noX)
{
  print("Nasz rekord przyjmuje klasê 'Yes'!")
} else if (P.buys.yesX < P.buys.noX)
{
  print("Nasz rekord przyjmuje klasê 'No'!")
}

# ------------------------------------------------------------------------

pcBuy <- data.frame("Age" = c("31-40", ">40", ">40", ">40", "31-40", "<=30", "<=30"),
           "Income" = c("High", "Medium", "High", "Low", "Low", "Medium", "Low"),
           "Student" = c("No", "No", "Yes", "Yes", "No", "No", "Yes"),
           "CreditRating" = c("Fair", "Fair", "Excellent", "Excellent", "Excellent", "Fair", "Fair"),
           "Buys" = c("Yes", "Yes", "Yes", "No", "Yes", "No", "No"))
pcBuy

checkIfBuys <- function(age, income, student, creditRating)
{
  stopifnot (age %in% c("<=30", "31-40", ">40"))
  stopifnot (income %in% c("Low", "Medium", "High"))
  stopifnot (student %in% c("Yes", "No"))
  stopifnot (creditRating %in% c("Fair", "Excellent"))
  
  P.buys.yes = nrow(subset(pcBuy, pcBuy["Buys"] == "Yes")) / nrow(pcBuy)
  P.buys.no = nrow(subset(pcBuy, pcBuy["Buys"] == "No")) / nrow(pcBuy)
  
  P.ageBuys.yes = nrow(subset(pcBuy, pcBuy["Age"] == age & pcBuy["Buys"] == "Yes")) / nrow(subset(pcBuy, pcBuy["Buys"] == "Yes"))
  P.ageBuys.no = nrow(subset(pcBuy, pcBuy["Age"] == age & pcBuy["Buys"] == "No")) / nrow(subset(pcBuy, pcBuy["Buys"] == "No"))
  
  P.incomeBuys.yes = nrow(subset(pcBuy, pcBuy["Income"] == income & pcBuy["Buys"] == "Yes")) / nrow(subset(pcBuy, pcBuy["Buys"] == "Yes"))
  P.incomeBuys.no = nrow(subset(pcBuy, pcBuy["Income"] == income & pcBuy["Buys"] == "No")) / nrow(subset(pcBuy, pcBuy["Buys"] == "No"))
  
  P.studentBuys.yes = nrow(subset(pcBuy, pcBuy["Student"] == student & pcBuy["Buys"] == "Yes")) / nrow(subset(pcBuy, pcBuy["Buys"] == "Yes"))
  P.studentBuys.no = nrow(subset(pcBuy, pcBuy["Student"] == student & pcBuy["Buys"] == "No")) / nrow(subset(pcBuy, pcBuy["Buys"] == "No"))
  
  P.creditrateBuys.yes = nrow(subset(pcBuy, pcBuy["CreditRating"] == creditRating & pcBuy["Buys"] == "Yes")) / nrow(subset(pcBuy, pcBuy["Buys"] == "Yes"))
  P.creditrateBuys.no = nrow(subset(pcBuy, pcBuy["CreditRating"] == creditRating & pcBuy["Buys"] == "No")) / nrow(subset(pcBuy, pcBuy["Buys"] == "No"))
  
  P.Xbuys.yes = P.ageBuys.yes * P.incomeBuys.yes * P.studentBuys.yes * P.creditrateBuys.yes
  P.Xbuys.no = P.ageBuys.no * P.incomeBuys.no * P.studentBuys.no * P.creditrateBuys.no
  
  P.buys.yesX = P.Xbuys.yes * P.buys.yes
  P.buys.noX = P.Xbuys.no * P.buys.no
  
  if (P.buys.yesX > P.buys.noX)
  {
    print("Nasz rekord przyjmuje klasê 'Yes'!")
    pcBuy <- rbind(pcBuy, data.frame("Age"=age, "Income"=income, "Student"=student, "CreditRating"=creditRating, "Buys"="Yes"))
  } else if (P.buys.yesX < P.buys.noX)
  {
    print("Nasz rekord przyjmuje klasê 'No'!")
    pcBuy <- rbind(pcBuy, data.frame("Age"=age, "Income"=income, "Student"=student, "CreditRating"=creditRating, "Buys"="No"))
  }

  return(pcBuy)
}

pcBuy <- checkIfBuys(">40", "Medium", "No", "Excellent")
pcBuy

pcBuy <- checkIfBuys(">40", "Low", "No", "Fair")
pcBuy

# ___ ZADANIE 3 __________________________________________________________

#install.packages("e1071")
#library("e1071")

set.seed(1234)
ind2 <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.norm.training2 <- iris.data.norm[ind2==1,1:5]
iris.norm.test2 <- iris.data.norm[ind2==2,1:5]

iris.bayes = naiveBayes(iris.norm.training2[,1:4],iris.norm.training2[,5])

predicted2 <- predict(iris.bayes, iris.norm.test[,1:4])
real2 <- iris.norm.test[,5]
conf.matrix2 <- table(predicted2,real2)
accuracy2 <- sum(diag(conf.matrix2))/sum(conf.matrix2)

conf.matrix2
accuracy2

accuracyComparison <- c("kNN (k=3)" = accuracy, "NaiveBayes" = accuracy2)

if (accuracyComparison[1] > accuracyComparison[2])
{
  paste("kNN (k=3), accuracy =", accuracyComparison[1], "IS BETTER THAN", "NaiveBayes, accuracy =", accuracyComparison[2])
} else if (accuracyComparison[1] > accuracyComparison[2]) {
  paste("NaiveBayes, accuracy =", accuracyComparison[2], "IS BETTER THAN", "kNN (k=3), accuracy =", accuracyComparison[1])
} else {
  paste("kNN (k=3), accuracy =", accuracyComparison[1], "IS EQUAL", "NaiveBayes, accuracy =", accuracyComparison[2])
}

# ------------------------------------------------------------------------