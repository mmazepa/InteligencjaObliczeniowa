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

# for (i in 1:nrow(drugs)) {
#   # ----- OBRÓBKA KOLUMNY "AGE" (WIEK) ---------------------------------
#   if (drugs["age"][i,] == -0.95197) drugs["age"][i,] <- "18-24"
#   else if (drugs["age"][i,] == -0.07854) drugs["age"][i,] <- "25-34"
#   else if (drugs["age"][i,] == 0.49788) drugs["age"][i,] <- "35-44"
#   else if (drugs["age"][i,] == 1.09449) drugs["age"][i,] <- "45-54"
#   else if (drugs["age"][i,] == 1.82213) drugs["age"][i,] <- "55-64"
#   else if (drugs["age"][i,] == 2.59171) drugs["age"][i,] <- "65+"
# 
#   # ----- OBRÓBKA KOLUMNY "GENDER" (P£EÆ) ------------------------------
#   if (drugs["gender"][i,] == 0.48246) drugs["gender"][i,] <- "Female"
#   else if (drugs["gender"][i,] == -0.48246) drugs["gender"][i,] <- "Male"
# 
#   # ----- OBRÓBKA KOLUMNY "EDUCATION" (EDUKACJA) -----------------------
#   if (drugs["education"][i,] == -2.43591) drugs["education"][i,] <- "Left school before 16 years"
#   else if (drugs["education"][i,] == -1.73790) drugs["education"][i,] <- "Left school at 16 years"
#   else if (drugs["education"][i,] == -1.43719) drugs["education"][i,] <- "Left school at 17 years"
#   else if (drugs["education"][i,] == -1.22751) drugs["education"][i,] <- "Left school at 18 years"
#   else if (drugs["education"][i,] == -0.61113) drugs["education"][i,] <- "Some college or university, no certificate or degree"
#   else if (drugs["education"][i,] == -0.05921) drugs["education"][i,] <- "Professional certificate/ diploma"
#   else if (drugs["education"][i,] == 0.45468) drugs["education"][i,] <- "University degree"
#   else if (drugs["education"][i,] == 1.16365) drugs["education"][i,] <- "Masters degree"
#   else if (drugs["education"][i,] == 1.98437) drugs["education"][i,] <- "Doctorate degree"
# 
#   # ----- OBRÓBKA KOLUMNY "COUNTRY" (KRAJ) -----------------------------
#   if (drugs["country"][i,] == -0.09765) drugs["country"][i,] <- "Australia"
#   else if (drugs["country"][i,] == 0.24923) drugs["country"][i,] <- "Canada"
#   else if (drugs["country"][i,] == -0.46841) drugs["country"][i,] <- "New Zealand"
#   else if (drugs["country"][i,] == -0.28519) drugs["country"][i,] <- "Other"
#   else if (drugs["country"][i,] == 0.21128) drugs["country"][i,] <- "Republic of Ireland"
#   else if (drugs["country"][i,] == 0.96082) drugs["country"][i,] <- "UK"
#   else if (drugs["country"][i,] == -0.57009) drugs["country"][i,] <- "USA"
# 
#   # ----- OBRÓBKA KOLUMNY "ETHNICITY" (NARODOWOŒÆ) ---------------------
#   if (drugs["ethnicity"][i,] == -0.50212) drugs["ethnicity"][i,] <- "Asian"
#   else if (drugs["ethnicity"][i,] == -1.10702) drugs["ethnicity"][i,] <- "Black"
#   else if (drugs["ethnicity"][i,] == 1.90725) drugs["ethnicity"][i,] <- "Mixed-Black/Asian"
#   else if (drugs["ethnicity"][i,] == 0.12600) drugs["ethnicity"][i,] <- "Mixed-White/Asian"
#   else if (drugs["ethnicity"][i,] == -0.22166) drugs["ethnicity"][i,] <- "Mixed-White/Black"
#   else if (drugs["ethnicity"][i,] == 0.11440) drugs["ethnicity"][i,] <- "Other"
#   else if (drugs["ethnicity"][i,] == -0.31685) drugs["ethnicity"][i,] <- "White"
# }

# ----- STATYSTYKI -------------------------------------------------------

statistics <- list("ages" = table(drugs["age"]),
                   "genders" = table(drugs["gender"]),
                   "education" = table(drugs["education"]),
                   "country" = table(drugs["country"]),
                   "ethnicity" = table(drugs["ethnicity"]))

# ------------------------------------------------------------------------