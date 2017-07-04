setwd(".")
options(error = recover, digits=4)
source(file="MSDS7333-Case08-fx.r")
library(lattice)
library(gplots)

#### Load Data ####
df = loadCherryBlossom10KMaleResults()
str(df)

# rows with missing data?
sum(is.na(df))
# remove rows with missing values
rowsWithMissingValues = unique (unlist (lapply (df, function (x) which (is.na (x)))))
df[rowsWithMissingValues, ]
# TODO: invetigate rows with missing data. how did they get there?
df = na.omit(df)

# factor year
df$year = factor(df$year)
# recommend age 10 year age groups per USATF standard
df$ageGrp = cut(df$age, c(0, 14, 19, 29, 39, 49, 59, 69, 79, 100))
# factor age group
df$ageGrp = factor(df$ageGrp)
# column summary
summary(df)

#### Age ####
# blox plots
bwplot(age ~ year, data=df, layout=c(1,1), pch="|", main="Age Distribution by Year", ylab="Age", xlab="Year")
# show records where age is <= 10
df[which(df$age<=10),]
# any age under 8 is nonsense, so remove
df = df[-c(which(df$age<8)), ]
# assert we have 65,990 rows
stopifnot(nrow(df)==65990)
#  review box plots again
bwplot(age ~ year, data=df, layout=c(1,1), pch="|", main="Age Distribution by Year", ylab="Age", xlab="Year")

#### Time ####
bwplot(time ~ year, data=df, layout=c(1,1), pch="|", main="Time Distribution by Year", ylab="Time", xlab="Year")
# show records where time is <= 50
df[which(df$time<=40),]
# any time under 20 is nonsense, so remove
df = df[-c(which(df$time<20)), ]
# assert we have 65,989 rows
stopifnot(nrow(df)==65989)
# review box plots again
bwplot(time ~ year, data=df, pch="|", main="Time Distribution by Year", ylab="Time", xlab="Year")

save(df, file="CBMenDf.rda")
