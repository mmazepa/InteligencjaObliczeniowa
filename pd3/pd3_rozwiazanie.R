# ------------------------------------------------------------------------
# PRACA DOMOWA 3
# ------------------------------------------------------------------------
# IMIÊ I NAZWISKO:   Mariusz Mazepa
# NUMER INDEKSU:     235371
# PRZEDMIOT:         Inteligencja Obliczeniowa
# GRUPA ÆWICZENIOWA: 3
# ZAGADNIENIE:       Sieci neuronowe steruj¹ce czo³gami
# ------------------------------------------------------------------------

# ----- PRZYGOTOWANIE BAZY DANYCH ----------------------------------------

library(readr)
file <- "C:/Users/Mariusz/Desktop/IO/Lab Repo/pd3/dane.csv"
dane <- read_csv(file, col_names = TRUE)
dane <- dane[1:250,]

# ----- POTRZEBNE PACZKI - NEURALNET -------------------------------------

#install.packages("neuralnet")
library(neuralnet)

# ----- NORMALIZACJA DANYCH ----------------------------------------------

norm <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}

dane.norm <- data.frame(norm(dane[1]), norm(dane[2]), norm(dane[3]),
                        norm(dane[4]), norm(dane[5]), norm(dane[6]),
                        norm(dane[7]), norm(dane[8]), norm(dane[9]),
                        norm(dane[10]), norm(dane[11]), norm(dane[12]),
                        norm(dane[13]), norm(dane[14]), norm(dane[15]),
                        norm(dane[16]), norm(dane[17]), norm(dane[18]),
                        norm(dane[19]), norm(dane[20]), norm(dane[21]),
                        norm(dane[22]), norm(dane[23]), norm(dane[24]),
                        norm(dane[25]), norm(dane[26]), norm(dane[27]),
                        norm(dane[28]), norm(dane[29]), norm(dane[30]),
                        norm(dane[31]), norm(dane[32]), norm(dane[33]),
                        norm(dane[34]), norm(dane[35]), norm(dane[36]),
                        norm(dane[37]), norm(dane[38]), norm(dane[39]),
                        norm(dane[40]), norm(dane[41]), norm(dane[42]),
                        norm(dane[43]), norm(dane[44]), norm(dane[45]),
                        norm(dane[46]), norm(dane[47]), norm(dane[48]),
                        norm(dane[49]), norm(dane[50]))

# ----- PODZIA£ NA ZBIÓR TRENINGOWY I TESTOWY ----------------------------

set.seed(1234)
ind <- sample(2, nrow(dane.norm), replace=TRUE, prob=c(0.67, 0.33))
dane.train <- dane.norm[ind==1,1:50]
dane.test <- dane.norm[ind==2,1:50]

# ----- POZBYCIE SIÊ WARTOŒCI NaN (NOT A NUMBER) -------------------------

is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}
dane.train[is.nan(dane.train)] <- 0
dane.test[is.nan(dane.test)] <- 0

# ----- NEURALNET - EWALUACJA KLASYFIKATORA ------------------------------

dane.neuralnet <- neuralnet(myTank.controls.turnLeft +     # (1)
                            myTank.controls.turnRight +    # (2)
                            myTank.controls.goForward +    # (3)
                            myTank.controls.goBack +       # (4)
                            myTank.controls.shoot +        # (5)
                            myTank.controls.cannonLeft +   # (6)
                            myTank.controls.cannonRight    # (7)
                            ~
                            myTank.x +
                            myTank.y +
                            myTank.rotation +
                            myTank.cannonRotation +
                            myTank.velocityX +
                            myTank.velocityY +
                            myTank.accelerationX +
                            myTank.accelerationY +
                            myTank.shootCooldown +
                            myBullet1.x +
                            myBullet1.y +
                            myBullet1.velocityX +
                            myBullet1.velocityY +
                            myBullet2.x +
                            myBullet2.y +
                            myBullet2.velocityX +
                            myBullet2.velocityY +
                            myBullet3.x +
                            myBullet3.y +
                            myBullet3.velocityX +
                            myBullet3.velocityY +
                            enemyTank.x +
                            enemyTank.y +
                            enemyTank.rotation +
                            enemyTank.cannonRotation +
                            enemyTank.velocityX +
                            enemyTank.velocityY +
                            enemyTank.accelerationX +
                            enemyTank.accelerationY +
                            enemyTank.shootCooldown +
                            enemyBullet1.x +
                            enemyBullet1.y +
                            enemyBullet1.velocityX +
                            enemyBullet1.velocityY +
                            enemyBullet2.x +
                            enemyBullet2.y +
                            enemyBullet2.velocityX +
                            enemyBullet2.velocityY +
                            enemyBullet3.x +
                            enemyBullet3.y +
                            enemyBullet3.velocityX +
                            enemyBullet3.velocityY +
                            currentGameTime,
                            dane.train, hidden=20)
plot(dane.neuralnet)

# ------------------------------------------------------------------------