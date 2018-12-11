# ------------------------------------------------------------------------
# PRACA DOMOWA 2
# ------------------------------------------------------------------------
# IMIÊ I NAZWISKO:   Mariusz Mazepa
# NUMER INDEKSU:     235371
# PRZEDMIOT:         Inteligencja Obliczeniowa
# GRUPA ÆWICZENIOWA: 3
# ZAGADNIENIE:       Zg³êbianie danych
# WYBRANA BAZA:      Drug Consumption
# ------------------------------------------------------------------------

#(trzeba by zmieniæ klasê z kilku wartoœci, na dwie wartoœci – potrzebna
#sensowna obróbka, albo wykonanie kilku eksperymentów z ró¿nymi klasami)
#(trudniejsze)

# ----- PRZYGOTOWANIE BAZY DANYCH ----------------------------------------

library(readr)
file <- "C:/Users/Mariusz/Desktop/IO/Lab Repo/pd2/drug_consumption.data"
drugs <- read_csv(file, col_names = FALSE)

names <- c("id", "age", "gender", "education", "country", "ethnicity",
           "nscore", "escore", "oscore", "ascore", "cscore", "impulsive",
           "ss", "alcohol", "amphet", "amyl", "benzos", "caff", "cannabis",
           "choc", "coke", "crack", "ecstasy", "heroin", "ketamine", "legalh",
           "lsd", "meth", "mushrooms", "nicotine", "semer", "vsa")
drugs <- setNames(drugs, names)
drugs2 <- drugs

# ----- OBRÓBKA BAZY DANYCH ----------------------------------------------

for (i in 1:nrow(drugs)) {
  # ----- OBRÓBKA KOLUMNY "AGE" (WIEK) ---------------------------------
  if (drugs["age"][i,] == -0.95197) drugs["age"][i,] <- "18-24"
  else if (drugs["age"][i,] == -0.07854) drugs["age"][i,] <- "25-34"
  else if (drugs["age"][i,] == 0.49788) drugs["age"][i,] <- "35-44"
  else if (drugs["age"][i,] == 1.09449) drugs["age"][i,] <- "45-54"
  else if (drugs["age"][i,] == 1.82213) drugs["age"][i,] <- "55-64"
  else if (drugs["age"][i,] == 2.59171) drugs["age"][i,] <- "65+"

  # ----- OBRÓBKA KOLUMNY "GENDER" (P£EÆ) ------------------------------
  if (drugs["gender"][i,] == 0.48246) drugs["gender"][i,] <- "Female"
  else if (drugs["gender"][i,] == -0.48246) drugs["gender"][i,] <- "Male"

  # ----- OBRÓBKA KOLUMNY "EDUCATION" (EDUKACJA) -----------------------
  if (drugs["education"][i,] == -2.43591) drugs["education"][i,] <- "Left school before 16 years"
  else if (drugs["education"][i,] == -1.73790) drugs["education"][i,] <- "Left school at 16 years"
  else if (drugs["education"][i,] == -1.43719) drugs["education"][i,] <- "Left school at 17 years"
  else if (drugs["education"][i,] == -1.22751) drugs["education"][i,] <- "Left school at 18 years"
  else if (drugs["education"][i,] == -0.61113) drugs["education"][i,] <- "Some college or university, no certificate or degree"
  else if (drugs["education"][i,] == -0.05921) drugs["education"][i,] <- "Professional certificate/ diploma"
  else if (drugs["education"][i,] == 0.45468) drugs["education"][i,] <- "University degree"
  else if (drugs["education"][i,] == 1.16365) drugs["education"][i,] <- "Masters degree"
  else if (drugs["education"][i,] == 1.98437) drugs["education"][i,] <- "Doctorate degree"

  # ----- OBRÓBKA KOLUMNY "COUNTRY" (KRAJ) -----------------------------
  if (drugs["country"][i,] == -0.09765) drugs["country"][i,] <- "Australia"
  else if (drugs["country"][i,] == 0.24923) drugs["country"][i,] <- "Canada"
  else if (drugs["country"][i,] == -0.46841) drugs["country"][i,] <- "New Zealand"
  else if (drugs["country"][i,] == -0.28519) drugs["country"][i,] <- "Other"
  else if (drugs["country"][i,] == 0.21128) drugs["country"][i,] <- "Republic of Ireland"
  else if (drugs["country"][i,] == 0.96082) drugs["country"][i,] <- "UK"
  else if (drugs["country"][i,] == -0.57009) drugs["country"][i,] <- "USA"

  # ----- OBRÓBKA KOLUMNY "ETHNICITY" (NARODOWOŒÆ) ---------------------
  if (drugs["ethnicity"][i,] == -0.50212) drugs["ethnicity"][i,] <- "Asian"
  else if (drugs["ethnicity"][i,] == -1.10702) drugs["ethnicity"][i,] <- "Black"
  else if (drugs["ethnicity"][i,] == 1.90725) drugs["ethnicity"][i,] <- "Mixed-Black/Asian"
  else if (drugs["ethnicity"][i,] == 0.12600) drugs["ethnicity"][i,] <- "Mixed-White/Asian"
  else if (drugs["ethnicity"][i,] == -0.22166) drugs["ethnicity"][i,] <- "Mixed-White/Black"
  else if (drugs["ethnicity"][i,] == 0.11440) drugs["ethnicity"][i,] <- "Other"
  else if (drugs["ethnicity"][i,] == -0.31685) drugs["ethnicity"][i,] <- "White"
}

# ----- POZBYCIE SIÊ NIEPOTRZEBNYCH DANYCH -------------------------------

drugs.alcohol <- drugs2[c(2:13,14)] # wybranie u¿ywki "alcohol"
drugs.alcohol2 <- drugs.alcohol
drugs.alcohol2$alcoholNum <- NA

# ----- MODYFIKACJA KOLUMNY Z KLAS¥ (WARTOŒCI: 6, MA BYÆ: 2) -------------

for (i in 1:nrow(drugs.alcohol)) {
  nonuserSet <- c("CL0", "CL1")
  userSet <- c("CL2", "CL3", "CL4", "CL5", "CL6")
  if (drugs.alcohol["alcohol"][i,] %in% nonuserSet) {
    drugs.alcohol["alcohol"][i,] <- "Non-User"
  } else if (drugs.alcohol["alcohol"][i,] %in% userSet) {
    drugs.alcohol["alcohol"][i,] <- "User"
  }
}

# ----- MODYFIKACJA KOLUMNY Z KLAS¥, WERSJA DLA CTREE --------------------

for (i in 1:nrow(drugs.alcohol2)) {
  nonuserSet <- c("CL0", "CL1")
  userSet <- c("CL2", "CL3", "CL4", "CL5", "CL6")
  if (drugs.alcohol2["alcohol"][i,] %in% nonuserSet) {
    drugs.alcohol2["alcoholNum"][i,] <- 0
  } else if (drugs.alcohol2["alcohol"][i,] %in% userSet) {
    drugs.alcohol2["alcoholNum"][i,] <- 1
  }
}

drugs.alcohol2$alcohol <- drugs.alcohol2$alcoholNum
drugs.alcohol2$alcoholNum <- NULL

# ----- WYKRESY ----------------------------------------------------------

#install.packages("plotrix")
#library(plotrix)

# --- SPO¯YCIE ALKOHOLU W ZALE¯NOŒCI OD P£CI ----

drugsGender <- data.frame("Gender" <- drugs["gender"],
                          "Class" <- drugs.alcohol["alcohol"])

tabDG <- table(drugsGender)
round(tabDG/sum(tabDG)*100, 2)

drugsGenderVal <- c(tabDG[1], tabDG[2], tabDG[3], tabDG[4])
drugsGenderNames <- c("Female Non-user",
                      "Male Non-user",
                      "Female user",
                      "Male user")

pie(drugsGenderVal, labels = drugsGenderNames,
    col = c("yellow", "green", "orange", "darkgreen"),
    main="Spo¿ycie alkoholu w zale¿noœci od p³ci")

# --- SPO¯YCIE ALKOHOLU W ZALE¯NOŒCI OD WIEKU ---

drugsAge <- data.frame("Age" <- drugs["age"],
                       "Class" <- drugs.alcohol["alcohol"])

tabDA <- table(drugsAge)
round(tabDA/sum(tabDA)*100, 2)

barplot(t(table(drugsAge)),
        main="Spo¿ywanie alkoholu w zale¿noœci od wieku",
        xlab="Przedzia³ wiekowy",
        ylab="Liczba ludzi")

barplot(table(drugsAge)[,1],
        main="Niespo¿ywanie alkoholu w zale¿noœci od wieku",
        xlab="Przedzia³ wiekowy",
        ylab="Liczba ludzi nieu¿ywaj¹cych alkoholu")

barplot(table(drugsAge)[,2],
        main="Spo¿ywanie alkoholu w zale¿noœci od wieku",
        xlab="Przedzia³ wiekowy",
        ylab="Liczba ludzi u¿ywaj¹cych alkoholu")

# --- SPO¯YCIE ALKOHOLU W ZALE¯NOŒCI OD EDUKACJI ---

drugsEducation <- data.frame("Education" <- drugs["education"],
                             "Class" <- drugs.alcohol["alcohol"])

tabDEdu <- table(drugsEducation)
round(tabDEdu/sum(tabDEdu)*100, 2)

par(mar=c(5, 20, 5, 1))
barplot(t(table(drugsEducation)),
        las=1,
        main="Spo¿ywanie alkoholu w zale¿noœci od wykszta³cenia",
        xlab="Liczba ludzi",
        horiz = TRUE)

# --- SPO¯YCIE ALKOHOLU W ZALE¯NOŒCI OD KRAJU ---

drugsCountry <- data.frame("Country" <- drugs["country"],
                           "Class" <- drugs.alcohol["alcohol"])

tabDC <- table(drugsCountry)
round(tabDC/sum(tabDC)*100, 2)

barplot(t(table(drugsCountry)),
        main="Spo¿ywanie alkoholu w zale¿noœci od kraju",
        xlab="Kraj",
        ylab="Liczba ludzi")

barplot(table(drugsCountry)[,1],
        main="Niespo¿ywanie alkoholu w zale¿noœci od kraju",
        xlab="Kraj",
        ylab="Liczba ludzi nieu¿ywaj¹cych alkoholu")

barplot(table(drugsCountry)[,2],
        main="Spo¿ywanie alkoholu w zale¿noœci od kraju",
        xlab="Kraj",
        ylab="Liczba ludzi u¿ywaj¹cych alkoholu")

# --- SPO¯YCIE ALKOHOLU W ZALE¯NOŒCI OD NARODOWOŒCI ---

drugsEthnicity <- data.frame("Ethnicity" <- drugs["ethnicity"],
                           "Class" <- drugs.alcohol["alcohol"])

tabDEth <- table(drugsEthnicity)
round(tabDEth/sum(tabDEth)*100, 2)

par(mar=c(5, 10, 5, 1))
barplot(t(table(drugsEthnicity)),
        las=1,
        main="Spo¿ywanie alkoholu w zale¿noœci od narodowoœci",
        xlab="Liczba ludzi",
        horiz = TRUE)

# ----- NORMALIZACJA DANYCH ----------------------------------------------

norm <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

drugs.norm <- data.frame(norm(drugs.alcohol[1]), norm(drugs.alcohol[2]),
                         norm(drugs.alcohol[3]), norm(drugs.alcohol[4]),
                         norm(drugs.alcohol[5]), norm(drugs.alcohol[6]),
                         norm(drugs.alcohol[7]), norm(drugs.alcohol[8]),
                         norm(drugs.alcohol[9]), norm(drugs.alcohol[10]),
                         norm(drugs.alcohol[11]), norm(drugs.alcohol[12]),
                         drugs.alcohol[13])

drugs.norm2 <- data.frame(norm(drugs.alcohol2[1]), norm(drugs.alcohol2[2]),
                         norm(drugs.alcohol2[3]), norm(drugs.alcohol2[4]),
                         norm(drugs.alcohol2[5]), norm(drugs.alcohol2[6]),
                         norm(drugs.alcohol2[7]), norm(drugs.alcohol2[8]),
                         norm(drugs.alcohol2[9]), norm(drugs.alcohol2[10]),
                         norm(drugs.alcohol2[11]), norm(drugs.alcohol2[12]),
                         drugs.alcohol2[13])

# ----- PODZIA£ NA ZBIÓR TESTOWY I TRENINGOWY ----------------------------

set.seed(1234)
ind <- sample(2, nrow(drugs.norm), replace=TRUE, prob=c(0.67, 0.33))
drugs.train <- drugs.norm[ind==1,1:13]
drugs.test <- drugs.norm[ind==2,1:13]

# ----- PODZIA£ NA ZBIÓR TESTOWY I TRENINGOWY V.2 ------------------------

set.seed(1234)
ind <- sample(2, nrow(drugs.norm2), replace=TRUE, prob=c(0.67, 0.33))
drugs.train2 <- drugs.norm2[ind==1,1:13]
drugs.test2 <- drugs.norm2[ind==2,1:13]

# ----- KLASYFIKATOR C4.5/ID3 (DRZEWO) -----------------------------------

#install.packages("party")
#library(party)

drugs.ctree <- ctree(factor(alcohol) ~ age + gender + education + country + ethnicity
                     + nscore + escore + oscore + ascore + cscore + impulsive + ss,
                     data=drugs.train)
print(drugs.ctree)
plot(drugs.ctree)
plot(drugs.ctree, type="simple")

tree.predicted <- predict(drugs.ctree, drugs.test[,1:12])
tree.real <- drugs.test[,13]
tree.conf.matrix <- table(tree.predicted, tree.real)
tree.accuracy <- sum(diag(tree.conf.matrix))/sum(tree.conf.matrix)

# ----- KLASYFIKATOR KNN -------------------------------------------------

#install.packages("class")
#library(class)

knn.3 <- knn(drugs.train[,1:12], drugs.test[,1:12], cl=drugs.train[,13], k=3, prob=FALSE)

knn.predicted <- knn.3
knn.real <- drugs.test[,13]
knn.conf.matrix <- table(knn.predicted, knn.real)
knn.accuracy <- sum(diag(knn.conf.matrix))/sum(knn.conf.matrix)

# ----- KLASYFIKATOR NAIVEBAYES ------------------------------------------

#install.packages("e1071")
#library(e1071)

nbayes <- naiveBayes(drugs.train[,1:12], drugs.train[,13])
nbayes$levels <- c("Non-user", "User")

nbayes.predicted <- predict(nbayes, drugs.test[,1:12])
nbayes.real <- drugs.test[,13]
nbayes.conf.matrix <- table(nbayes.predicted, nbayes.real)
nbayes.accuracy <- sum(diag(nbayes.conf.matrix))/sum(nbayes.conf.matrix)

# ----- WYKRES DOK£ADNOŒCI -----------------------------------------------

accuracies <- c("C4.5/ID3 (drzewo)" = tree.accuracy,
                "kNN" = knn.accuracy,
                "NaiveBayes" = nbayes.accuracy) # jeszcze jeden inny
accuracyPlot <- barplot(accuracies,
        main="Dok³adnoœci klasyfikatorów",
        xlab="Klasyfikator", ylab="Dok³adnoœæ klasyfikatora")
text(x = accuracyPlot, y = accuracies, label = round(accuracies,5),
     pos = 1, cex = 0.8, col = "navy")

# ----- PARY (TPR, FPR) --------------------------------------------------

# TPR = TP/(TP+FN)
# FPR = FP/(FP+TN)

tree.tpr <- tree.conf.matrix[1]/(tree.conf.matrix[1] + tree.conf.matrix[2])
tree.fpr <- tree.conf.matrix[3]/(tree.conf.matrix[3] + tree.conf.matrix[4])

knn.tpr <- knn.conf.matrix[1]/(knn.conf.matrix[1] + knn.conf.matrix[2])
knn.fpr <- knn.conf.matrix[3]/(knn.conf.matrix[3] + knn.conf.matrix[4])

nbayes.tpr <- nbayes.conf.matrix[1]/(nbayes.conf.matrix[1] + nbayes.conf.matrix[2])
nbayes.fpr <- nbayes.conf.matrix[3]/(nbayes.conf.matrix[3] + nbayes.conf.matrix[4])

tprsAndFprs <- data.frame(
  "C4.5/ID3 (drzewo)" = c("TPR" = tree.tpr, "FPR" = tree.fpr),
  "kNN" = c("TPR" = knn.tpr, "FPR" = knn.fpr),
  "NaiveBayes" = c("TPR" = nbayes.tpr, "FPR" = nbayes.fpr)
)

# ----- WYKRES ROC SPACE -------------------------------------------------

rocX <- c(tree.tpr, knn.tpr, nbayes.tpr)
rocY <- c(tree.fpr, knn.fpr, nbayes.fpr)
names <- c("C4.5/ID3 (drzewo)", "kNN", "NaiveBayes")

plot(rocX, rocY, main="ROC Space",
     xlab="False Positive Rate (FPR)",
     ylab="True Positive Rate (TPR)",
     col="red")
text(rocX, rocY, labels=names, cex= 0.7, pos=c(4,4,2))

# ----- GRUPOWANIE METOD¥ K-ŒREDNICH -------------------------------------

#install.packages("editrules")
#library(editrules)

drugs.log <- log(drugs.norm[,1:12])
drugs.log <- drugs.log[is.finite(rowSums(drugs.log)),]
drugs.log$gender <- NULL

drugs.scale <- scale(drugs.log, center=TRUE)
drugs.pca <- prcomp(drugs.scale)
drugs.final <- predict(drugs.pca)
drugs.final <- drugs.final[,1:11]

drugs.kmeans <- kmeans(drugs.final, 2)

dev.off() # par(...) reset
plot(drugs.final, col = drugs.kmeans[["cluster"]],
     main="Algorytm grupowania metod¹ k-œrednich")
points(drugs.kmeans[["centers"]], col = 1:2, pch = 16, cex=1.5)

# ----- REGU£Y ASOCJACYJNE -----------------------------------------------

library(arules)

raDrugs <- drugs[,2:6]
raDrugs$alcohol <- drugs.alcohol$alcohol

raDrugs$age <- factor(raDrugs$age)
raDrugs$gender <- factor(raDrugs$gender)
raDrugs$education <- factor(raDrugs$education)
raDrugs$country <- factor(raDrugs$country)
raDrugs$ethnicity <- factor(raDrugs$ethnicity)
raDrugs$alcohol <- factor(raDrugs$alcohol)

rules <- apriori(raDrugs)
inspect(rules)

# ----- REGU£Y ASOCJACYJNE: SELEKCJA ------------------------------------

rules2 <- apriori(raDrugs,
                 appearance = list(rhs=c("alcohol=User"),
                 default="lhs"),
                 control = list(verbose=F))
rules2 <- sort(rules2, by="lift")
inspect(rules2)

# ----- REGU£Y ASOCJACYJNE GRAFICZNIE ------------------------------------

library(arulesViz)
plot(rules[1:10], method="graph", control=list(type="items"))

# ----- STATYSTYKI -------------------------------------------------------

statistics <- list("ages" = table(drugs["age"]),
                   "genders" = table(drugs["gender"]),
                   "education" = table(drugs["education"]),
                   "country" = table(drugs["country"]),
                   "ethnicity" = table(drugs["ethnicity"]),
                   "alcohol" = table(drugs.alcohol["alcohol"]))

# ------------------------------------------------------------------------