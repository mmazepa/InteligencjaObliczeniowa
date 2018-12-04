# LABORATORIUM 10 [04.12.2018]
# ------------------------------------------------------------------------

# ___ ZADANIE 1 __________________________________________________________

#install.packages("sets")
#library(sets)

N <- fuzzy_normal(mean = 0, sd = 2)
N(-5:5)

fset.A <- gset(charfun = N, universe = -5:5)
plot(fset.A)

# ...

# ------------------------------------------------------------------------

ma <- function(x) {
  if (x > 2 & x <= 4) return(0)
  else return(1)
}

mb <- function(x) {
  if (x > 3 & x <= 5) return(1)
  else if (x > 1 & x <= 3) return(0.5*x - 0.5)
  else if (x > 5 & x <= 6) return(-x + 6)
  else return(0)
}

mc <- function(x) {
  if (x > 0 & x <= 1) return(x)
  else if (x > 1 & x <= 3) return(-0.5*x + 1.5)
  else return(0)
}

# ...

# ___ ZADANIE 2 __________________________________________________________

#                +-------+---------+-----+
#                | start | stabil. | fin |
# +--------------+-------+---------+-----+
# | "m³odoœæ"    |     0 |     18  |  30 |
# | "dojrza³oœæ" |    18 |     30  |  70 |
# | "staroœæ"    |    60 |     70  | 130 |
# +--------------+-------+---------+-----+

# ...

# ___ ZADANIE 3 __________________________________________________________

#install.packages("sets")
#library(sets)

sets_options("universe", seq(from = 0, to = 40, by = 0.1))

variables <- set(
  bmi =
    fuzzy_partition(varnames =
                      c(niedow = 9.25, zdro = 21.75,
                        nadw = 27.5, otyl = 35),
                        sd = 3.0),
  a1c =
    fuzzy_partition(varnames =
                      c(nisk = 4, norm = 5.25, wys = 7),
                      FUN = fuzzy_cone, radius = 5),
  rating =
    fuzzy_partition(varnames =
                      c(odm = 10, stand = 5, pref = 1),
                      FUN = fuzzy_cone, radius = 5),
  bp =
    fuzzy_partition(varnames =
                      c(norm = 0, mnadcis = 10, nadcis = 20,
                        dnadci = 30), sd = 2.5)
)

rules <-
  set(
    fuzzy_rule(bmi %is% niedow || bmi %is% otyl || a1c %is% nisk, rating %is% odm),
    fuzzy_rule(bmi %is% nadw || a1c %is% nisk || bp %is% mnadcis, rating %is% stand),
    fuzzy_rule(bmi %is% zdro && a1c %is% norm && bp %is% norm, rating %is% pref)
  )

system <- fuzzy_system(variables, rules)

print(system)
plot(system)

fi <- fuzzy_inference(system, list(bmi = 29, a1c=5, bp=20))
plot(fi)

gset_defuzzify(fi, "centroid") # bli¿ej "stand", bo < 7.5

# ------------------------------------------------------------------------