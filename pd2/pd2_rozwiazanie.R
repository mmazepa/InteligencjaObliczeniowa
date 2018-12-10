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

# ----- OBRÓBKA BAZY DANYCH ----------------------------------------------

# R Studio (and Eclipse + StatET): Highlight the text and use CTRL+SHIFT+C
# to comment multiple lines in Windows.

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

drugs.alcohol <- drugs[c(2:13,14)] # wybranie u¿ywki "alcohol"
drugs.alcohol$alcoholNum <- NA

# ----- MODYFIKACJA KOLUMNY Z KLAS¥ (WARTOŒCI: 6, MA BYÆ: 2) -------------

for (i in 1:nrow(drugs.alcohol)) {
  nonuserSet <- c("CL0", "CL1")
  userSet <- c("CL2", "CL3", "CL4", "CL5", "CL6")
  if (drugs.alcohol["alcohol"][i,] %in% nonuserSet) {
    drugs.alcohol["alcohol"][i,] <- "Non-User"
    drugs.alcohol["alcoholNum"][i,] <- 0
  } else if (drugs.alcohol["alcohol"][i,] %in% userSet) {
    drugs.alcohol["alcohol"][i,] <- "User"
    drugs.alcohol["alcoholNum"][i,] <- 1
  }
}

drugs.alcohol$alcohol <- drugs.alcohol$alcoholNum
drugs.alcohol$alcoholNum <- NULL

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

# --- SPO¯YCIE ALKOHOLU W ZALE¯NOŒCI OD KRAJU ---

drugsCountry <- data.frame("Country" <- drugs["country"],
                           "Class" <- drugs.alcohol["alcohol"])

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

# ----- PODZIA£ NA ZBIÓR TESTOWY I TRENINGOWY ----------------------------

set.seed(1234)
ind <- sample(2, nrow(drugs.norm), replace=TRUE, prob=c(0.67, 0.33))
drugs.train <- drugs.norm[ind==1,1:13]
drugs.test <- drugs.norm[ind==2,1:13]

# ----- KLASYFIKATOR C4.5/ID3 (DRZEWO) -----------------------------------

#install.packages("party")
#library(party)

drugs.ctree <- ctree(alcohol ~ age + gender + education + country + ethnicity
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

nbayes = naiveBayes(drugs.train[,1:12], drugs.train[,13])

nbayes.predicted <- predict(nbayes, drugs.test[,1:12])
nbayes.real <- drugs.test[,13]
nbayes.conf.matrix <- table(nbayes.predicted, nbayes.real)
nbayes.accuracy <- sum(diag(nbayes.conf.matrix))/sum(nbayes.conf.matrix)

# ----- WYKRES DOK£ADNOŒCI -----------------------------------------------

barplot(c("C4.5/ID3" = tree.accuracy,
          "kNN" = knn.accuracy,
          "NaiveBayes" = 0), # jeszcze jeden inny
        main="Dok³adnoœci klasyfikatorów",
        xlab="Klasyfikatory", ylab="Dok³adnoœæ")

# ----- GRUPOWANIE METOD¥ K-ŒREDNICH -------------------------------------

#install.packages("editrules")
#library(editrules)

drugs.log <- log(drugs.norm[,1:12])
#drugs.log <- do.call(data.frame, lapply(drugs.log, function(x) replace(x, is.infinite(x), 0)))
#drugs.log <- do.call(data.frame, lapply(drugs.log, function(x) replace(x, x == 0, 0.5)))

drugs.scale <- scale(drugs.log, center=TRUE)
drugs.pca <- prcomp(drugs.scale)
drugs.final <- predict(drugs.pca)
#drugs.final <- iris.final[,1:2]

drugs.kmeans <- kmeans(drugs.final, 3)

plot(drugs.final, col = drugs.kmeans[["cluster"]], main="Algorytm grupowania")
points(drugs.kmeans[["centers"]], col = 1:3, pch = 16, cex=1.5)

# ----- STATYSTYKI -------------------------------------------------------

statistics <- list("ages" = table(drugs["age"]),
                   "genders" = table(drugs["gender"]),
                   "education" = table(drugs["education"]),
                   "country" = table(drugs["country"]),
                   "ethnicity" = table(drugs["ethnicity"]),
                   "alcohol" = table(drugs.alcohol["alcohol"]))

# ------------------------------------------------------------------------