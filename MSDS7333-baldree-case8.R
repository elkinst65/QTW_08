setwd(".")
options(error = recover, digits=4)
source(file="MSDS7333-baldree-case8-fx.r")
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
# histogram
histogram(~age|year, data=df, main="Histogram Plot by Year", xlab="Age")
# density plot
densityplot(~age|year, data=df,  main="Density Plot by Year", xlab="Age")
# qq plot
qqmath(~age|year, data=df, main="QQ Plot by Year", xlab="qnorm", ylab="Age", cex=.3)

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
bwplot(time ~ ageGrp|year, data=df, pch="|", main="Time Distribution by Age Group", ylab="Time",
       xlab="Age Group", scales=list(x=list(draw=FALSE)))
# histogram
histogram(~time|year, data=df, main="Histogram Plot by Year", xlab="Time")
# density plot
densityplot(~time|year, data=df, main="Density Plot by Year", xlab="Time")
# qq plot
qqmath(~time|year, data=df, main="QQ Plot by Year", xlab="qnorm", ylab="Time", cex=.3)

#### Age v Time ####
xyplot(time~age|year, data=df, cex=.3, xlab="Age", ylab="Time", main="Time v Age by Year")
xyplot(time~age|year, data=df, cex=.3, type=c("r"), xlab="Age", ylab="Time", main="Time v Age Regression by Year")

# avg time and age per year
yrAvg = aggregate(cbind(age, time) ~ year, data=df, FUN=mean)
xyplot(time~age, data=yrAvg, pch=20, type=c("p", "r"),
       main="Average Time for Average Age", xlab="Age", ylab="Time",
       panel=function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.text(x, y, labels=c(1999:2012), pos=1, offset=1, cex=0.8)
       })

xyplot(age~year, data=yrAvg, pch=20, type=c("p", "smooth"),
       main="Average Age by Year", xlab="Year", ylab="Age")

xyplot(time~year, data=yrAvg, pch=20, type=c("p", "smooth"),
       main="Average Time by Year", xlab="Year", ylab="Time")

yrAvgAgeGrp = aggregate(cbind(age, time) ~ year, data=df, FUN=mean, subset=ageGrp=="(39,49]")
xyplot(time~year, data=yrAvgAgeGrp, pch=20, type=c("p", "smooth"),
       main="Average Time by Year for Age Group 40-49", xlab="Year", ylab="Time")

# stddev time and age per year
yrStdDev = aggregate(cbind(age, time) ~ year, data=df, FUN=sd)
xyplot(time~age, data=yrStdDev, pch=20, type=c("p", "r"),
       main="Std. Dev. Time versus Std. Dev. Age", xlab="Age", ylab="Time",
       panel=function(x, y, ...){
         panel.xyplot(x, y, ...)
         panel.text(x, y, labels=c(1999:2012), pos=1, offset=1, cex=0.8)
       })

#### ANOVA ####
# more than two groups, so we need to use ANOVA
fit = aov(age ~ year, data=df)
summary(fit)
rfs(fit)
par(mai=c(1, 1, .5, .2), mfrow=c(1,1))
plotmeans(age ~ year, data=df, xlab="Year", ylab="Age", main="Mean Plot with 95% CI", n.label=FALSE)

# looks a time and block for age group as well
fit = aov(time ~ year+ageGrp, data=df)
summary(fit)
rfs(fit)
par(mai=c(1, 1, .5, .2), mfrow=c(1,1))
plotmeans(time ~ year, data=df, xlab="Year", ylab="Time", main="Mean Plot with 95% CI", n.label=FALSE)

#### MANOVA ####
# more than two groups, so we need to use ANOVA
fit = manova(cbind(age, time) ~ year, data=df)
summary(fit)
rfs(fit)
par(mai=c(1, 1, .5, .2), mfrow=c(1,1))
plotmeans(cbind(age, time) ~ year, data=df, xlab="Year", ylab="Age and Time", main="Mean Plot with 95% CI", n.label=FALSE)
