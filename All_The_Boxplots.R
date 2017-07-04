bwplot(time ~ ageGrp|year, data=df, pch="|", main="Time Distribution by Age Group", ylab="Time",
       xlab="Age Group", scales=list(x=list(draw=FALSE)))

# The overall goal is to display a boxplot of times by agegroup for every year. 
# This will be animated by replotting the boxplots each year in a for-loop
# It will be saved with saveGIF then embedded into the powerpoint. 
# This will be done to show how the age groups have changed over the years of the race. 

df2001 <- df$age[df$year == 2001]
boxplot(df2001, main = "2001", horizontal = TRUE)

# What if we manually generate a separate box  plot for each year and just loop through them? 

df1999 <- df$age[df$year == 1999]
b99 <- boxplot(df1999, ylim = c(10,89), main = "Age Distribution for 1999", horizontal = TRUE)

df2000 <- df$age[df$year == 2000]
b00 <- boxplot(df2000, ylim = c(10,89), main = "Age Distribution for 2000", horizontal = TRUE)

df2001 <- df$age[df$year == 2001]
b1 <- boxplot(df2001, ylim = c(10,89), main = "Age Distribution for 2001", horizontal = TRUE)

df2002 <- df$age[df$year == 2002]
b2 <- boxplot(df2002, ylim = c(10,89), main = "Age Distribution for 2002", horizontal = TRUE)

df2003 <- df$age[df$year == 2003]
b3 <- boxplot(df2003, ylim = c(10,89), main = "Age Distribution for 2003", horizontal = TRUE)

df2004 <- df$age[df$year == 2004]
b4 <- boxplot(df2004, ylim = c(10,89), main = "Age Distribution for 2004", horizontal = TRUE)

df2005 <- df$age[df$year == 2005]
b5 <- boxplot(df2005, ylim = c(10,89), main = "Age Distribution for 2005", horizontal = TRUE)

df2006 <- df$age[df$year == 2006]
b6 <- boxplot(df2006, ylim = c(10,89), main = "Age Distribution for 2006", horizontal = TRUE)

df2007 <- df$age[df$year == 2007]
b7 <- boxplot(df2007, ylim = c(10,89), main = "Age Distribution for 2007", horizontal = TRUE)

df2008 <- df$age[df$year == 2008]
b8 <- boxplot(df2008, ylim = c(10,89), main = "Age Distribution for 2008", horizontal = TRUE)

df2009 <- df$age[df$year == 2009]
b9 <- boxplot(df2009, ylim = c(10,89), main = "Age Distribution for 2009", horizontal = TRUE)

df2010 <- df$age[df$year == 2010]
b10 <- boxplot(df2010, ylim = c(10,89), main = "Age Distribution for 2010", horizontal = TRUE)

df2011 <- df$age[df$year == 2011]
b11 <- boxplot(df2011, ylim = c(10,89), main = "Age Distribution for 2011", horizontal = TRUE)

df2012 <- df$age[df$year == 2012]
b12 <- boxplot(df2012, ylim = c(10,89), main = "Age Distribution for 2012", horizontal = TRUE)
