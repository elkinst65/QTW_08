setwd(".")
options(error = recover, digits=4)
source(file="MSDS7333-baldree-case8-fx.r")

ubase = "http://www.cherryblossom.org/"

# menURLs =
#   c("results/2001/oof_m.html",
#     "results/2002/oofm.htm", "results/2003/CB03-M.HTM",
#     "results/2004/men.htm", "results/2005/CB05-M.htm",
#     "results/2006/men.htm", "results/2007/men.htm",
#     "results/2008/men.htm", "results/2009/09cucb-M.htm",
#     "results/2010/2010cucb10m-m.htm",
#     "results/2011/2011cucb10m-m.htm",
#     "results/2012/2012cucb10m-m.htm")

menURLs = c("results/2001/oof_m.html",
            "results/2002/oofm.htm",
            "results/2003/CB03-M.HTM",
            "results/2004/men.htm",
            "results/2005/CB05-M.htm",
            "results/2006/men.htm",
            "results/2007/men.htm",
            "results/2008/men.htm",
            "results/2009/09cucb-M.htm",
            "results/2010/2010cucb10m-m.htm",
            "results/2011/2011cucb10m-m.htm",
            "results/2012/2012cucb10m-m.htm")

urls = paste(ubase, menURLs, sep = "")
urls[1:12]

years = 2001:2012
# extract data from web pages
menTables = mapply(extractResTable, url = urls, year = years)
# name each table
names(menTables) = years
# print number of rows
sapply(menTables, length)


# fix 2006
separatorIdx = grep("^===", menTables[["2006"]])
separatorRow = menTables[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ",
                      substring(separatorRow, 65, nchar(separatorRow)),
                      sep = "")
menTables[['2006']][separatorIdx] = separatorRowX

# extract variables
menResMat = lapply(menTables, extractVariables)
# create a list of data frames
menDF = mapply(createDF, menResMat, year = years, sex = rep("M", length(years)), SIMPLIFY = FALSE)
# merge data frames
menDF = Reduce(merge.all, menDF)

# visualization
library(lattice)
bwplot(age ~ factor(year), data=menDF, layout=c(1,1), main="Age Distribution by Year", ylab="Age", xlab="Year")

save(menTables, file = "CBMenTextTables.rda")
