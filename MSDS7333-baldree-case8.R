setwd(".")
options(error = recover, digits=4)
source(file="MSDS7333-baldree-case8-fx.r")
library(lattice)

#### Load Data ####
df = loadCherryBlossom10KMaleResults()
str(df)

# rows with missing data?
sum(is.na(df))
# TODO: invetigate rows with missing data. how did they get there?

# column summary
summary(df)

# factor year
df$year = factor(df$year)

#### Questions ####
#
# Is is correct that we don't have a timeseries because these are independent samples taken
# each race?
#

#### Age ####
# blox plots
bwplot(age ~ year, data=df, layout=c(1,1), pch="|", main="Age Distribution by Year", ylab="Age", xlab="Year")
# show records where age is <= 10
df[which(df$age<=10),]
# any age under 8 is nonsense, so remove
df = df[-c(which(df$age<8)), ]
# assert we have 66,010 rows
stopifnot(nrow(df)==66010)
#  review box plots again
bwplot(age ~ year, data=df, layout=c(1,1), pch="|", main="Age Distribution by Year", ylab="Age", xlab="Year")
# histogram
histogram(~age|year, data=df, main="Histogram Plot by Year", xlab="Age")
# density plot
densityplot(~age|year, data=df,  main="Density Plot by Year", xlab="Age")
# qq plot
qqmath(~age|year, data=df, main="QQ Plot by Year", xlab="qnorm", ylab="Age", cex=.3)
# TODO: ANOVA
model = lm(age ~ year, data=df)
summary(model)
  # TODO: year 1999 not showing up. why?
anova(model)
confint(model)
rfs(model)


#### Time ####
bwplot(time ~ year, data=df, layout=c(1,1), pch="|", main="Time Distribution by Year", ylab="Time", xlab="Year")
# show records where time is <= 50
df[which(df$time<=40),]
# any time under 20 is nonsense, so remove
df = df[-c(which(df$time<20)), ]
# assert we have 66,009 rows
stopifnot(nrow(df)==66009)
# review box plots again
bwplot(time ~ year, data=df, layout=c(1,1), pch="|", main="Time Distribution by Year", ylab="Time", xlab="Year")
# histogram
histogram(~time|year, data=df, main="Histogram Plot by Year", xlab="Time")
# density plot
densityplot(~time|year, data=df, main="Density Plot by Year", xlab="Time")
# qq plot
qqmath(~time|year, data=df, main="QQ Plot by Year", xlab="qnorm", ylab="Time", cex=.3)
# ANOVA
model = lm(time ~ year, data=df)
summary(model)
anova(model)
rfs(model)


#### Age v Time ####
xyplot(time~age|year, data=df, cex=.3)
xyplot(time~age|year, data=df, cex=.3, type=c("r"))

