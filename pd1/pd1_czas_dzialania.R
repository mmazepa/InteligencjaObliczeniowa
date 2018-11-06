# ------------------------------------------------------------------------
# funkcja fitness: 10 klauzul, 5 zmiennych
fitnessFunc2 <- function(chr)
{
  # budowanie formuły
  f <- c()
  f[1] <- (!chr[1] | chr[2]  | chr[4])      #  1-sza klauzula
  f[2] <- (!chr[2] | chr[3]  | chr[4])      #  2-ga  klauzula
  f[3] <- (chr[3]  | !chr[4] | chr[5])      #  3-cia klauzula
  f[4] <- (chr[2]  | !chr[3] | !chr[4])     #  4-ta  klauzula
  f[5] <- (chr[3]  | !chr[4] | !chr[5])     #  5-ta  klauzula
  f[6] <- (!chr[3] | chr[4]  | !chr[5])     #  6-ta  klauzula
  f[7] <- (chr[1]  | chr[2]  | chr[4])      #  7-ma  klauzula
  f[8] <- (chr[2]  | !chr[4] | !chr[5])     #  8-ma  klauzula
  f[9] <- (!chr[3] | chr[4]  | !chr[5])     #  9-ta  klauzula
  f[10] <- (chr[1]  | chr[2]  | chr[4])     # 10-ta  klauzula
  
  # iterowanie po formule, zliczanie jedynek
  ilosc_jedynek <- 0;
  for (klauzula in f)
  {
    if (klauzula == TRUE) ilosc_jedynek = ilosc_jedynek + 1
  }
  # ocena chromosomu
  return(-ilosc_jedynek)
}

# funkcja fitness: 20 klauzul, 10 zmiennych
fitnessFunc3 <- function(chr)
{
  # budowanie formuły
  f <- c()
  f[1] <- (!chr[1] | chr[9]  | chr[10])     #  1-sza klauzula
  f[2] <- (!chr[2] | chr[8]  | chr[4])      #  2-ga  klauzula
  f[3] <- (chr[3]  | !chr[7] | chr[5])      #  3-cia klauzula
  f[4] <- (chr[4]  | !chr[6] | !chr[9])     #  4-ta  klauzula
  f[5] <- (chr[5]  | !chr[4] | !chr[8])     #  5-ta  klauzula
  f[6] <- (!chr[6] | chr[5]  | !chr[7])     #  6-ta  klauzula
  f[7] <- (chr[7]  | chr[3]  | chr[4])      #  7-ma  klauzula
  f[8] <- (chr[8]  | !chr[2] | !chr[5])     #  8-ma  klauzula
  f[9] <- (!chr[7] | chr[1]  | !chr[5])     #  9-ta  klauzula
  f[10] <- (chr[6]  | chr[2]  | chr[10])    # 10-ta  klauzula
  f[11] <- (!chr[5] | chr[3]  | chr[4])     # 11-ta  klauzula
  f[12] <- (!chr[4] | chr[5]  | chr[1])     # 12-ta  klauzula
  f[13] <- (chr[3]  | !chr[4] | chr[5])     # 13-ta klauzula
  f[14] <- (chr[2]  | !chr[6] | !chr[4])    # 14-ta  klauzula
  f[15] <- (chr[1]  | !chr[7] | !chr[5])    # 15-ta  klauzula
  f[16] <- (!chr[2] | chr[8]  | !chr[10])   # 16-ta  klauzula
  f[17] <- (chr[3]  | chr[2]  | chr[4])     # 17-ta  klauzula
  f[18] <- (chr[4]  | !chr[7] | !chr[5])    # 18-ta  klauzula
  f[19] <- (!chr[5] | chr[4]  | !chr[8])    # 19-ta  klauzula
  f[20] <- (chr[6]  | chr[2]  | chr[4])     # 20-ta  klauzula
  
  # iterowanie po formule, zliczanie jedynek
  ilosc_jedynek <- 0;
  for (klauzula in f)
  {
    if (klauzula == TRUE) ilosc_jedynek = ilosc_jedynek + 1
  }
  # ocena chromosomu
  return(-ilosc_jedynek)
}

# funkcja fitness: 40 klauzul, 20 zmiennych
fitnessFunc4 <- function(chr)
{
  # budowanie formuły
  f <- c()
  f[1] <- (!chr[1] | chr[9]  | chr[10])     #  1-sza klauzula
  f[2] <- (!chr[2] | chr[8]  | chr[14])     #  2-ga  klauzula
  f[3] <- (chr[3]  | !chr[7] | chr[5])      #  3-cia klauzula
  f[4] <- (chr[4]  | !chr[6] | !chr[9])     #  4-ta  klauzula
  f[5] <- (chr[5]  | !chr[14] | !chr[18])   #  5-ta  klauzula
  f[6] <- (!chr[6] | chr[15]  | !chr[17])   #  6-ta  klauzula
  f[7] <- (chr[7]  | chr[3]  | chr[4])      #  7-ma  klauzula
  f[8] <- (chr[8]  | !chr[12] | !chr[5])    #  8-ma  klauzula
  f[9] <- (!chr[7] | chr[1]  | !chr[5])     #  9-ta  klauzula
  f[10] <- (chr[6]  | chr[2]  | chr[10])    # 10-ta  klauzula
  f[11] <- (!chr[5] | chr[13]  | chr[4])    # 11-ta  klauzula
  f[12] <- (!chr[4] | chr[5]  | chr[1])     # 12-ta  klauzula
  f[13] <- (chr[3]  | !chr[4] | chr[5])     # 13-ta  klauzula
  f[14] <- (chr[2]  | !chr[6] | !chr[4])    # 14-ta  klauzula
  f[15] <- (chr[1]  | !chr[7] | !chr[5])    # 15-ta  klauzula
  f[16] <- (!chr[7] | chr[18]  | !chr[20])  # 16-ta  klauzula
  f[17] <- (chr[3]  | chr[2]  | chr[4])     # 17-ta  klauzula
  f[18] <- (chr[4]  | !chr[17] | !chr[5])   # 18-ta  klauzula
  f[19] <- (!chr[10] | chr[14]  | !chr[18]) # 19-ta  klauzula
  f[20] <- (chr[6]  | chr[12]  | chr[4])    # 20-ta  klauzula
  f[21] <- (!chr[5] | chr[3]  | chr[4])     # 21-sza klauzula
  f[22] <- (!chr[4] | chr[5]  | chr[1])     # 22-ga  klauzula
  f[23] <- (chr[3]  | !chr[4] | chr[5])     # 23-cia klauzula
  f[24] <- (chr[2]  | !chr[6] | !chr[4])    # 24-ta  klauzula
  f[25] <- (chr[1]  | !chr[7] | !chr[5])    # 25-ta  klauzula
  f[26] <- (!chr[2] | chr[18]  | !chr[10])  # 26-ta  klauzula
  f[27] <- (chr[13]  | chr[2]  | chr[4])    # 27-ma  klauzula
  f[28] <- (chr[4]  | !chr[17] | !chr[5])   # 28-ma  klauzula
  f[29] <- (!chr[5] | chr[4]  | !chr[8])    # 29-ta  klauzula
  f[30] <- (chr[6]  | chr[2]  | chr[4])     # 30-ta  klauzula
  f[31] <- (!chr[5] | chr[13]  | chr[4])    # 31-sza klauzula
  f[32] <- (!chr[4] | chr[5]  | chr[1])     # 32-ga  klauzula
  f[33] <- (chr[3]  | !chr[4] | chr[20])    # 33-cia klauzula
  f[34] <- (chr[2]  | !chr[6] | !chr[4])    # 34-ta  klauzula
  f[35] <- (chr[11]  | !chr[7] | !chr[5])   # 35-ta  klauzula
  f[36] <- (!chr[2] | chr[8]  | !chr[10])   # 36-ta  klauzula
  f[37] <- (chr[13]  | chr[2]  | chr[4])    # 37-ma  klauzula
  f[38] <- (chr[4]  | !chr[7] | !chr[5])    # 38-ma  klauzula
  f[39] <- (!chr[5] | chr[4]  | !chr[18])   # 39-ta  klauzula
  f[40] <- (chr[16]  | chr[2]  | chr[4])    # 40-ta  klauzula
  
  # iterowanie po formule, zliczanie jedynek
  ilosc_jedynek <- 0;
  for (klauzula in f)
  {
    if (klauzula == TRUE) ilosc_jedynek = ilosc_jedynek + 1
  }
  # ocena chromosomu
  return(-ilosc_jedynek)
}

# funkcja fitness: 60 klauzul, 30 zmiennych
fitnessFunc5 <- function(chr)
{
  # budowanie formuły
  f <- c()
  f[1] <- (!chr[1] | chr[29]  | chr[20])    #  1-sza klauzula
  f[2] <- (!chr[2] | chr[8]  | chr[14])     #  2-ga  klauzula
  f[3] <- (chr[3]  | !chr[7] | chr[5])      #  3-cia klauzula
  f[4] <- (chr[4]  | !chr[6] | !chr[9])     #  4-ta  klauzula
  f[5] <- (chr[25]  | !chr[14] | !chr[18])  #  5-ta  klauzula
  f[6] <- (!chr[6] | chr[15]  | !chr[17])   #  6-ta  klauzula
  f[7] <- (chr[7]  | chr[3]  | chr[4])      #  7-ma  klauzula
  f[8] <- (chr[18]  | !chr[22] | !chr[28])  #  8-ma  klauzula
  f[9] <- (!chr[7] | chr[1]  | !chr[5])     #  9-ta  klauzula
  f[10] <- (chr[6]  | chr[2]  | chr[10])    # 10-ta  klauzula
  f[11] <- (!chr[5] | chr[13]  | chr[4])    # 11-ta  klauzula
  f[12] <- (!chr[4] | chr[5]  | chr[1])     # 12-ta  klauzula
  f[13] <- (chr[3]  | !chr[4] | chr[5])     # 13-ta  klauzula
  f[14] <- (chr[2]  | !chr[6] | !chr[4])    # 14-ta  klauzula
  f[15] <- (chr[13]  | !chr[27] | !chr[25]) # 15-ta  klauzula
  f[16] <- (!chr[27] | chr[28]  | !chr[30]) # 16-ta  klauzula
  f[17] <- (chr[3]  | chr[2]  | chr[4])     # 17-ta  klauzula
  f[18] <- (chr[4]  | !chr[17] | !chr[5])   # 18-ta  klauzula
  f[19] <- (!chr[10] | chr[14]  | !chr[18]) # 19-ta  klauzula
  f[20] <- (chr[6]  | chr[12]  | chr[4])    # 20-ta  klauzula
  f[21] <- (!chr[5] | chr[3]  | chr[4])     # 21-sza klauzula
  f[22] <- (!chr[4] | chr[5]  | chr[1])     # 22-ga  klauzula
  f[23] <- (chr[3]  | !chr[4] | chr[5])     # 23-cia klauzula
  f[24] <- (chr[2]  | !chr[6] | !chr[4])    # 24-ta  klauzula
  f[25] <- (chr[1]  | !chr[7] | !chr[5])    # 25-ta  klauzula
  f[26] <- (!chr[2] | chr[18]  | !chr[10])  # 26-ta  klauzula
  f[27] <- (chr[13]  | chr[2]  | chr[4])    # 27-ma  klauzula
  f[28] <- (chr[4]  | !chr[17] | !chr[5])   # 28-ma  klauzula
  f[29] <- (!chr[11] | chr[21]  | !chr[30]) # 29-ta  klauzula
  f[30] <- (chr[6]  | chr[2]  | chr[4])     # 30-ta  klauzula
  f[31] <- (!chr[5] | chr[13]  | chr[4])    # 31-sza klauzula
  f[32] <- (!chr[4] | chr[5]  | chr[1])     # 32-ga  klauzula
  f[33] <- (chr[3]  | !chr[4] | chr[20])    # 33-cia klauzula
  f[34] <- (chr[2]  | !chr[6] | !chr[4])    # 34-ta  klauzula
  f[35] <- (chr[11]  | !chr[7] | !chr[5])   # 35-ta  klauzula
  f[36] <- (!chr[2] | chr[8]  | !chr[10])   # 36-ta  klauzula
  f[37] <- (chr[13]  | chr[2]  | chr[4])    # 37-ma  klauzula
  f[38] <- (chr[4]  | !chr[7] | !chr[5])    # 38-ma  klauzula
  f[39] <- (!chr[5] | chr[4]  | !chr[18])   # 39-ta  klauzula
  f[40] <- (chr[16]  | chr[2]  | chr[4])    # 40-ta  klauzula
  f[41] <- (!chr[5] | chr[3]  | chr[4])     # 41-sza klauzula
  f[42] <- (!chr[4] | chr[5]  | chr[1])     # 42-ga  klauzula
  f[43] <- (chr[3]  | !chr[4] | chr[5])     # 43-cia klauzula
  f[44] <- (chr[2]  | !chr[6] | !chr[4])    # 44-ta  klauzula
  f[45] <- (chr[1]  | !chr[7] | !chr[5])    # 45-ta  klauzula
  f[46] <- (!chr[2] | chr[18]  | !chr[10])  # 46-ta  klauzula
  f[47] <- (chr[13]  | chr[2]  | chr[4])    # 47-ma  klauzula
  f[48] <- (chr[4]  | !chr[27] | !chr[5])   # 48-ma  klauzula
  f[49] <- (!chr[5] | chr[4]  | !chr[8])    # 49-ta  klauzula
  f[50] <- (chr[6]  | chr[2]  | chr[4])     # 50-ta  klauzula
  f[51] <- (!chr[5] | chr[13]  | chr[4])    # 51-sza klauzula
  f[52] <- (!chr[4] | chr[5]  | chr[1])     # 52-ga  klauzula
  f[53] <- (chr[3]  | !chr[4] | chr[20])    # 53-cia klauzula
  f[54] <- (chr[2]  | !chr[6] | !chr[4])    # 54-ta  klauzula
  f[55] <- (chr[11]  | !chr[17] | !chr[25]) # 55-ta  klauzula
  f[56] <- (!chr[2] | chr[8]  | !chr[10])   # 56-ta  klauzula
  f[57] <- (chr[13]  | chr[2]  | chr[4])    # 57-ma  klauzula
  f[58] <- (chr[4]  | !chr[7] | !chr[5])    # 58-ma  klauzula
  f[59] <- (!chr[5] | chr[4]  | !chr[18])   # 59-ta  klauzula
  f[60] <- (chr[26]  | chr[2]  | chr[24])   # 60-ta  klauzula
  
  # iterowanie po formule, zliczanie jedynek
  ilosc_jedynek <- 0;
  for (klauzula in f)
  {
    if (klauzula == TRUE) ilosc_jedynek = ilosc_jedynek + 1
  }
  # ocena chromosomu
  return(-ilosc_jedynek)
}

# funkcja fitness: 80 klauzul, 40 zmiennych
fitnessFunc6 <- function(chr)
{
  # budowanie formuły
  f <- c()
  f[1] <- (!chr[1] | chr[29]  | chr[20])    #  1-sza klauzula
  f[2] <- (!chr[2] | chr[8]  | chr[14])     #  2-ga  klauzula
  f[3] <- (chr[3]  | !chr[7] | chr[5])      #  3-cia klauzula
  f[4] <- (chr[4]  | !chr[6] | !chr[9])     #  4-ta  klauzula
  f[5] <- (chr[25]  | !chr[14] | !chr[18])  #  5-ta  klauzula
  f[6] <- (!chr[6] | chr[15]  | !chr[17])   #  6-ta  klauzula
  f[7] <- (chr[7]  | chr[3]  | chr[4])      #  7-ma  klauzula
  f[8] <- (chr[18]  | !chr[22] | !chr[38])  #  8-ma  klauzula
  f[9] <- (!chr[7] | chr[1]  | !chr[5])     #  9-ta  klauzula
  f[10] <- (chr[6]  | chr[32]  | chr[10])   # 10-ta  klauzula
  f[11] <- (!chr[5] | chr[13]  | chr[4])    # 11-ta  klauzula
  f[12] <- (!chr[4] | chr[5]  | chr[40])    # 12-ta  klauzula
  f[13] <- (chr[3]  | !chr[4] | chr[5])     # 13-ta  klauzula
  f[14] <- (chr[2]  | !chr[6] | !chr[4])    # 14-ta  klauzula
  f[15] <- (chr[13]  | !chr[27] | !chr[25]) # 15-ta  klauzula
  f[16] <- (!chr[27] | chr[28]  | !chr[30]) # 16-ta  klauzula
  f[17] <- (chr[3]  | chr[2]  | chr[4])     # 17-ta  klauzula
  f[18] <- (chr[4]  | !chr[17] | !chr[5])   # 18-ta  klauzula
  f[19] <- (!chr[10] | chr[14]  | !chr[18]) # 19-ta  klauzula
  f[20] <- (chr[16]  | chr[22]  | chr[34])  # 20-ta  klauzula
  f[21] <- (!chr[5] | chr[3]  | chr[4])     # 21-sza klauzula
  f[22] <- (!chr[4] | chr[5]  | chr[1])     # 22-ga  klauzula
  f[23] <- (chr[3]  | !chr[4] | chr[5])     # 23-cia klauzula
  f[24] <- (chr[2]  | !chr[6] | !chr[4])    # 24-ta  klauzula
  f[25] <- (chr[1]  | !chr[7] | !chr[5])    # 25-ta  klauzula
  f[26] <- (!chr[2] | chr[18]  | !chr[10])  # 26-ta  klauzula
  f[27] <- (chr[13]  | chr[2]  | chr[4])    # 27-ma  klauzula
  f[28] <- (chr[34]  | !chr[38] | !chr[40]) # 28-ma  klauzula
  f[29] <- (!chr[11] | chr[21]  | !chr[30]) # 29-ta  klauzula
  f[30] <- (chr[6]  | chr[2]  | chr[4])     # 30-ta  klauzula
  f[31] <- (!chr[5] | chr[13]  | chr[4])    # 31-sza klauzula
  f[32] <- (!chr[4] | chr[5]  | chr[1])     # 32-ga  klauzula
  f[33] <- (chr[3]  | !chr[4] | chr[20])    # 33-cia klauzula
  f[34] <- (chr[2]  | !chr[6] | !chr[4])    # 34-ta  klauzula
  f[35] <- (chr[11]  | !chr[7] | !chr[5])   # 35-ta  klauzula
  f[36] <- (!chr[2] | chr[8]  | !chr[10])   # 36-ta  klauzula
  f[37] <- (chr[13]  | chr[2]  | chr[4])    # 37-ma  klauzula
  f[38] <- (chr[4]  | !chr[7] | !chr[5])    # 38-ma  klauzula
  f[39] <- (!chr[5] | chr[4]  | !chr[18])   # 39-ta  klauzula
  f[40] <- (chr[17]  | chr[22]  | chr[34])  # 40-ta  klauzula
  f[41] <- (!chr[5] | chr[3]  | chr[4])     # 41-sza klauzula
  f[42] <- (!chr[4] | chr[35]  | chr[37])   # 42-ga  klauzula
  f[43] <- (chr[3]  | !chr[4] | chr[5])     # 43-cia klauzula
  f[44] <- (chr[2]  | !chr[6] | !chr[4])    # 44-ta  klauzula
  f[45] <- (chr[1]  | !chr[7] | !chr[5])    # 45-ta  klauzula
  f[46] <- (!chr[2] | chr[18]  | !chr[10])  # 46-ta  klauzula
  f[47] <- (chr[13]  | chr[2]  | chr[4])    # 47-ma  klauzula
  f[48] <- (chr[4]  | !chr[27] | !chr[5])   # 48-ma  klauzula
  f[49] <- (!chr[5] | chr[4]  | !chr[8])    # 49-ta  klauzula
  f[50] <- (chr[6]  | chr[2]  | chr[4])     # 50-ta  klauzula
  f[51] <- (!chr[5] | chr[13]  | chr[4])    # 51-sza klauzula
  f[52] <- (!chr[4] | chr[5]  | chr[1])     # 52-ga  klauzula
  f[53] <- (chr[3]  | !chr[34] | chr[40])   # 53-cia klauzula
  f[54] <- (chr[2]  | !chr[6] | !chr[4])    # 54-ta  klauzula
  f[55] <- (chr[11]  | !chr[17] | !chr[25]) # 55-ta  klauzula
  f[56] <- (!chr[2] | chr[8]  | !chr[10])   # 56-ta  klauzula
  f[57] <- (chr[13]  | chr[32]  | chr[40])  # 57-ma  klauzula
  f[58] <- (chr[4]  | !chr[7] | !chr[5])    # 58-ma  klauzula
  f[59] <- (!chr[5] | chr[4]  | !chr[18])   # 59-ta  klauzula
  f[60] <- (chr[26]  | chr[2]  | chr[24])   # 60-ta  klauzula
  f[61] <- (!chr[5] | chr[3]  | chr[4])     # 61-sza klauzula
  f[62] <- (!chr[4] | chr[5]  | chr[1])     # 62-ga  klauzula
  f[63] <- (chr[3]  | !chr[4] | chr[5])     # 63-cia klauzula
  f[64] <- (chr[2]  | !chr[16] | !chr[24])  # 64-ta  klauzula
  f[65] <- (chr[1]  | !chr[7] | !chr[5])    # 65-ta  klauzula
  f[66] <- (!chr[2] | chr[18]  | !chr[10])  # 66-ta  klauzula
  f[67] <- (chr[13]  | chr[2]  | chr[4])    # 67-ma  klauzula
  f[68] <- (chr[4]  | !chr[27] | !chr[5])   # 68-ma  klauzula
  f[69] <- (!chr[5] | chr[4]  | !chr[8])    # 69-ta  klauzula
  f[70] <- (chr[6]  | chr[2]  | chr[4])     # 70-ta  klauzula
  f[71] <- (!chr[5] | chr[13]  | chr[4])    # 71-sza klauzula
  f[72] <- (!chr[4] | chr[5]  | chr[1])     # 72-ga  klauzula
  f[73] <- (chr[3]  | !chr[4] | chr[20])    # 73-cia klauzula
  f[74] <- (chr[2]  | !chr[6] | !chr[4])    # 74-ta  klauzula
  f[75] <- (chr[11]  | !chr[17] | !chr[25]) # 75-ta  klauzula
  f[76] <- (!chr[2] | chr[8]  | !chr[10])   # 76-ta  klauzula
  f[77] <- (chr[13]  | chr[2]  | chr[4])    # 77-ma  klauzula
  f[78] <- (chr[4]  | !chr[7] | !chr[5])    # 78-ma  klauzula
  f[79] <- (!chr[5] | chr[4]  | !chr[18])   # 79-ta  klauzula
  f[80] <- (chr[26]  | chr[2]  | chr[24])   # 80-ta  klauzula
  
  # iterowanie po formule, zliczanie jedynek
  ilosc_jedynek <- 0;
  for (klauzula in f)
  {
    if (klauzula == TRUE) ilosc_jedynek = ilosc_jedynek + 1
  }
  # ocena chromosomu
  return(-ilosc_jedynek)
}

# funkcja fitness: 100 klauzul, 50 zmiennych
fitnessFunc7 <- function(chr)
{
  # budowanie formuły
  f <- c()
  f[1] <- (!chr[1] | chr[29]  | chr[20])    #   1-sza klauzula
  f[2] <- (!chr[2] | chr[8]  | chr[14])     #   2-ga  klauzula
  f[3] <- (chr[3]  | !chr[7] | chr[40])     #   3-cia klauzula
  f[4] <- (chr[4]  | !chr[6] | !chr[9])     #   4-ta  klauzula
  f[5] <- (chr[25]  | !chr[14] | !chr[18])  #   5-ta  klauzula
  f[6] <- (!chr[6] | chr[15]  | !chr[17])   #   6-ta  klauzula
  f[7] <- (chr[7]  | chr[3]  | chr[4])      #   7-ma  klauzula
  f[8] <- (chr[18]  | !chr[32] | !chr[48])  #   8-ma  klauzula
  f[9] <- (!chr[7] | chr[1]  | !chr[5])     #   9-ta  klauzula
  f[10] <- (chr[6]  | chr[32]  | chr[10])   #  10-ta  klauzula
  f[11] <- (!chr[5] | chr[13]  | chr[4])    #  11-ta  klauzula
  f[12] <- (!chr[4] | chr[45]  | chr[50])   #  12-ta  klauzula
  f[13] <- (chr[3]  | !chr[4] | chr[5])     #  13-ta  klauzula
  f[14] <- (chr[2]  | !chr[6] | !chr[4])    #  14-ta  klauzula
  f[15] <- (chr[13]  | !chr[27] | !chr[25]) #  15-ta  klauzula
  f[16] <- (!chr[27] | chr[28]  | !chr[30]) #  16-ta  klauzula
  f[17] <- (chr[3]  | chr[2]  | chr[4])     #  17-ta  klauzula
  f[18] <- (chr[4]  | !chr[17] | !chr[5])   #  18-ta  klauzula
  f[19] <- (!chr[10] | chr[14]  | !chr[18]) #  19-ta  klauzula
  f[20] <- (chr[16]  | chr[22]  | chr[34])  #  20-ta  klauzula
  f[21] <- (!chr[5] | chr[3]  | chr[4])     #  21-sza klauzula
  f[22] <- (!chr[4] | chr[5]  | chr[1])     #  22-ga  klauzula
  f[23] <- (chr[3]  | !chr[4] | chr[5])     #  23-cia klauzula
  f[24] <- (chr[2]  | !chr[6] | !chr[4])    #  24-ta  klauzula
  f[25] <- (chr[1]  | !chr[7] | !chr[5])    #  25-ta  klauzula
  f[26] <- (!chr[2] | chr[18]  | !chr[10])  #  26-ta  klauzula
  f[27] <- (chr[13]  | chr[2]  | chr[4])    #  27-ma  klauzula
  f[28] <- (chr[34]  | !chr[38] | !chr[40]) #  28-ma  klauzula
  f[29] <- (!chr[11] | chr[21]  | !chr[30]) #  29-ta  klauzula
  f[30] <- (chr[6]  | chr[2]  | chr[4])     #  30-ta  klauzula
  f[31] <- (!chr[5] | chr[13]  | chr[4])    #  31-sza klauzula
  f[32] <- (!chr[44] | chr[45]  | chr[48])  #  32-ga  klauzula
  f[33] <- (chr[3]  | !chr[4] | chr[20])    #  33-cia klauzula
  f[34] <- (chr[2]  | !chr[6] | !chr[4])    #  34-ta  klauzula
  f[35] <- (chr[11]  | !chr[47] | !chr[25]) #  35-ta  klauzula
  f[36] <- (!chr[2] | chr[8]  | !chr[10])   #  36-ta  klauzula
  f[37] <- (chr[13]  | chr[2]  | chr[4])    #  37-ma  klauzula
  f[38] <- (chr[4]  | !chr[7] | !chr[5])    #  38-ma  klauzula
  f[39] <- (!chr[5] | chr[4]  | !chr[18])   #  39-ta  klauzula
  f[40] <- (chr[17]  | chr[22]  | chr[34])  #  40-ta  klauzula
  f[41] <- (!chr[5] | chr[3]  | chr[4])     #  41-sza klauzula
  f[42] <- (!chr[4] | chr[35]  | chr[37])   #  42-ga  klauzula
  f[43] <- (chr[3]  | !chr[4] | chr[5])     #  43-cia klauzula
  f[44] <- (chr[2]  | !chr[6] | !chr[4])    #  44-ta  klauzula
  f[45] <- (chr[1]  | !chr[7] | !chr[5])    #  45-ta  klauzula
  f[46] <- (!chr[2] | chr[18]  | !chr[10])  #  46-ta  klauzula
  f[47] <- (chr[13]  | chr[2]  | chr[4])    #  47-ma  klauzula
  f[48] <- (chr[4]  | !chr[27] | !chr[5])   #  48-ma  klauzula
  f[49] <- (!chr[5] | chr[4]  | !chr[8])    #  49-ta  klauzula
  f[50] <- (chr[6]  | chr[2]  | chr[4])     #  50-ta  klauzula
  f[51] <- (!chr[5] | chr[13]  | chr[4])    #  51-sza klauzula
  f[52] <- (!chr[4] | chr[5]  | chr[1])     #  52-ga  klauzula
  f[53] <- (chr[3]  | !chr[34] | chr[40])   #  53-cia klauzula
  f[54] <- (chr[2]  | !chr[6] | !chr[4])    #  54-ta  klauzula
  f[55] <- (chr[11]  | !chr[17] | !chr[25]) #  55-ta  klauzula
  f[56] <- (!chr[2] | chr[8]  | !chr[10])   #  56-ta  klauzula
  f[57] <- (chr[13]  | chr[32]  | chr[40])  #  57-ma  klauzula
  f[58] <- (chr[4]  | !chr[7] | !chr[5])    #  58-ma  klauzula
  f[59] <- (!chr[5] | chr[4]  | !chr[18])   #  59-ta  klauzula
  f[60] <- (chr[26]  | chr[2]  | chr[24])   #  60-ta  klauzula
  f[61] <- (!chr[5] | chr[3]  | chr[4])     #  61-sza klauzula
  f[62] <- (!chr[4] | chr[5]  | chr[1])     #  62-ga  klauzula
  f[63] <- (chr[3]  | !chr[4] | chr[5])     #  63-cia klauzula
  f[64] <- (chr[2]  | !chr[16] | !chr[24])  #  64-ta  klauzula
  f[65] <- (chr[1]  | !chr[7] | !chr[50])   #  65-ta  klauzula
  f[66] <- (!chr[2] | chr[18]  | !chr[10])  #  66-ta  klauzula
  f[67] <- (chr[13]  | chr[2]  | chr[4])    #  67-ma  klauzula
  f[68] <- (chr[4]  | !chr[27] | !chr[5])   #  68-ma  klauzula
  f[69] <- (!chr[5] | chr[4]  | !chr[8])    #  69-ta  klauzula
  f[70] <- (chr[6]  | chr[2]  | chr[4])     #  70-ta  klauzula
  f[71] <- (!chr[5] | chr[13]  | chr[4])    #  71-sza klauzula
  f[72] <- (!chr[4] | chr[5]  | chr[1])     #  72-ga  klauzula
  f[73] <- (chr[3]  | !chr[4] | chr[50])    #  73-cia klauzula
  f[74] <- (chr[2]  | !chr[6] | !chr[4])    #  74-ta  klauzula
  f[75] <- (chr[11]  | !chr[17] | !chr[25]) #  75-ta  klauzula
  f[76] <- (!chr[2] | chr[8]  | !chr[10])   #  76-ta  klauzula
  f[77] <- (chr[13]  | chr[2]  | chr[4])    #  77-ma  klauzula
  f[78] <- (chr[4]  | !chr[7] | !chr[5])    #  78-ma  klauzula
  f[79] <- (!chr[5] | chr[4]  | !chr[18])   #  79-ta  klauzula
  f[80] <- (chr[26]  | chr[2]  | chr[24])   #  80-ta  klauzula
  f[81] <- (!chr[5] | chr[3]  | chr[4])     #  81-sza klauzula
  f[82] <- (!chr[4] | chr[5]  | chr[1])     #  82-ga  klauzula
  f[83] <- (chr[3]  | !chr[4] | chr[5])     #  83-cia klauzula
  f[84] <- (chr[2]  | !chr[16] | !chr[24])  #  84-ta  klauzula
  f[85] <- (chr[1]  | !chr[7] | !chr[5])    #  85-ta  klauzula
  f[86] <- (!chr[2] | chr[18]  | !chr[50])  #  86-ta  klauzula
  f[87] <- (chr[13]  | chr[2]  | chr[4])    #  87-ma  klauzula
  f[88] <- (chr[4]  | !chr[27] | !chr[50])  #  88-ma  klauzula
  f[89] <- (!chr[5] | chr[4]  | !chr[8])    #  89-ta  klauzula
  f[90] <- (chr[6]  | chr[2]  | chr[4])     #  90-ta  klauzula
  f[91] <- (!chr[5] | chr[13]  | chr[4])    #  91-sza klauzula
  f[92] <- (!chr[4] | chr[25]  | chr[31])   #  92-ga  klauzula
  f[93] <- (chr[3]  | !chr[4] | chr[20])    #  93-cia klauzula
  f[94] <- (chr[42]  | !chr[46] | !chr[48]) #  94-ta  klauzula
  f[95] <- (chr[11]  | !chr[17] | !chr[25]) #  95-ta  klauzula
  f[96] <- (!chr[12] | chr[38]  | !chr[50]) #  96-ta  klauzula
  f[97] <- (chr[13]  | chr[2]  | chr[4])    #  97-ma  klauzula
  f[98] <- (chr[4]  | !chr[7] | !chr[5])    #  98-ma  klauzula
  f[99] <- (!chr[5] | chr[4]  | !chr[18])   #  99-ta  klauzula
  f[100] <- (chr[36]  | chr[42]  | chr[50]) # 100-na  klauzula
  
  # iterowanie po formule, zliczanie jedynek
  ilosc_jedynek <- 0;
  for (klauzula in f)
  {
    if (klauzula == TRUE) ilosc_jedynek = ilosc_jedynek + 1
  }
  # ocena chromosomu
  return(-ilosc_jedynek)
}

# algorytm genetyczny z paczki "genalg" : mierzenie czasu działania
trisatGenAlg2 <- system.time(rbga.bin(size = 5, popSize = 200, iters = 100,
                  mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc2))
trisatGenAlg3 <- system.time(rbga.bin(size = 10, popSize = 200, iters = 100,
                  mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc3))
trisatGenAlg4 <- system.time(rbga.bin(size = 20, popSize = 200, iters = 100,
                  mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc4))
trisatGenAlg5 <- system.time(rbga.bin(size = 30, popSize = 200, iters = 100,
                  mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc5))
trisatGenAlg6 <- system.time(rbga.bin(size = 40, popSize = 200, iters = 100,
                  mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc6))
trisatGenAlg7 <- system.time(rbga.bin(size = 50, popSize = 200, iters = 100,
                  mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc7))

czasy <- c(trisatGenAlg2[["elapsed"]][1],
           trisatGenAlg3[["elapsed"]][1],
           trisatGenAlg4[["elapsed"]][1],
           trisatGenAlg5[["elapsed"]][1],
           trisatGenAlg6[["elapsed"]][1],
           trisatGenAlg7[["elapsed"]][1])
rozmiary <- c(10, 20, 40, 60, 80, 100)

plot(rozmiary, czasy, type="o", main="Czasy Działania Alg. Genetycznego",
     xlab="Liczba klauzul", xlim=c(10,100),
     ylab="Czas trwania obliczeń", ylim=c(1,11),
     pch=16, col="green", lab = c(10,10,0))
lines(rozmiary, czasy, type="p", pch=16, col="darkgreen")
text(rozmiary, czasy+0.5, labels=round(czasy,2), cex= 0.75)
# ------------------------------------------------------------------------
