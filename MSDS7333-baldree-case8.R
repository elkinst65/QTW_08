setwd(".")
options(error = recover, digits=4)
source(file="MSDS7333-baldree-case8-fx.r")

df = loadCherryBlossom10KMaleResults()

# visualization
library(lattice)
bwplot(age ~ factor(year), data=df, layout=c(1,1), main="Age Distribution by Year", ylab="Age", xlab="Year")

# save off data
save(menTables, file = "CBMenTextTables.rda")
