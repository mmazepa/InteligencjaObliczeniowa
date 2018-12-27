# LABORATORIUM 8 [20.11.2018]
# ------------------------------------------------------------------------

# ___ ZADANIE 1 __________________________________________________________

a2.support <- 4/10
a2.confidence <- a2.support / (9/10)

a3.support <- 5/10
a3.confidence <- a3.support / (7/10)
  
a4.support <- 5/10
a4.confidence <- a4.support / (5/10)
  
a5.support <- 2/10
a5.confidence <- a5.support / (2/10)

# ___ ZADANIE 2 __________________________________________________________

str(titanic.raw)
#install.packages("arules")
#library(arules)
rules <- apriori(titanic.raw)
inspect(rules)

rules <- apriori(titanic.raw,
    parameter = list(minlen=2, supp=0.005, conf=0.8),
    appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"),
    control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#install.packages("arulesViz")
#library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))

# ------------------------------------------------------------------------