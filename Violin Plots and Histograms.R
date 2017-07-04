# Let's do Histograms by Year

# This plots all histograms at once. Maybe we don't need a gif of this...
histogram(~age|year, data=df, xlim = c(8,89), main="Histogram Plot by Year", xlab="Age")

age <- df$age
year <- df$year
xlim = c(8,89)
ylim = c(0, 1550)
xlab <- "Runner Age"

age99 <- age[year ==  1999]
age00 <- age[year ==  2000]
age01 <- age[year ==  2001]
age02 <- age[year ==  2002]
age03 <- age[year ==  2003]
age04 <- age[year ==  2004]
age05 <- age[year ==  2005]
age06 <- age[year ==  2006]
age07 <- age[year ==  2007]
age08 <- age[year ==  2008]
age09 <- age[year ==  2009]
age10 <- age[year ==  2010]
age11 <- age[year ==  2011]
age12 <- age[year ==  2012]

main99 <- "Distribution of ages for 1999"
main00 <- "Distribution of ages for 2000"
main01 <- "Distribution of ages for 2001"
main02 <- "Distribution of ages for 2002"
main03 <- "Distribution of ages for 2003"
main04 <- "Distribution of ages for 2004"
main05 <- "Distribution of ages for 2005"
main06 <- "Distribution of ages for 2006"
main07 <- "Distribution of ages for 2007"
main08 <- "Distribution of ages for 2008"
main09 <- "Distribution of ages for 2009"
main10 <- "Distribution of ages for 2010"
main11 <- "Distribution of ages for 2011"
main12 <- "Distribution of ages for 2012"


col = "yellow"
border <- "red3"

hist(age99, main = main99, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age00, main = main00, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age01, main = main01, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age02, main = main02, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age03, main = main03, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age04, main = main04, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age05, main = main05, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age06, main = main06, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age07, main = main07, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age08, main = main08, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age09, main = main09, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age10, main = main10, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age11, main = main11, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")
hist(age12, main = main12, xlim = xlim, col = col,border = border, xlab = xlab,ylab = "Number of Runners")



# Let's try Density plots by year
densityplot(~age|year, data = df)

#  Let's do violin plots for ages by year
library(vioplot)
v1 <- df$time[df$year == 1999]
v2 <- df$time[df$year == 2000]
v3 <- df$time[df$year == 2001]
v4 <- df$time[df$year == 2002]
v5 <- df$time[df$year == 2003]
v6 <- df$time[df$year == 2004]
v7 <- df$time[df$year == 2005]
v8 <- df$time[df$year == 2006]
v9 <- df$time[df$year == 2007]
v10 <- df$time[df$year == 2008]
v11 <- df$time[df$year == 2009]
v12 <- df$time[df$year == 2010]
v13 <- df$time[df$year == 2011]
v14 <- df$time[df$year == 2012]

par(bg = "gray80")

summary(df$time)

vioplot(v1, names = "1999", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v2, names = "2000", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v3, names = "2001", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v4, names = "2002", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v5, names = "2003", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v6, names = "2004", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v7, names = "2005", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v8, names = "2006", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v9, names = "2007", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v10, names = "2008", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v11, names = "2009", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v12, names = "2010", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v13, names = "2011", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")

vioplot(v14, names = "2012", col = "orange1", ylim = c(45,175))
title(ylab = "Time in Minutes", main="Run Time by Year")
