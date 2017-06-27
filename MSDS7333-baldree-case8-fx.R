loadCherryBlossom10KMaleResults = function() {
  #
  # load 10K male results from 1999 to 2012
  #
  ubase = "http://www.cherryblossom.org/"

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
  # drop unnecessary columns
  menDF = subset(menDF, select=-c(name, home))
  # rename column
  colnames(menDF)[colnames(menDF)=="runTime"] = "net"

  # load 1999 data
  # load Runners data set from statistical modeling R package
  library(ggformula)
  library(devtools)
  devtools::install_github("dtkaplan/statisticalModeling")
  library(statisticalModeling)
  data(Runners)
  menDF.1999 = Runners[Runners$year=="1999",]
  # drop unnecessary columns
  menDF.1999 = subset(menDF.1999, select=-c(start_position, previous, nruns, net))
  # drop females
  menDF.1999 = menDF.1999[!(menDF.1999$sex=="F"),]
  # rename columns
  colnames(menDF.1999)[colnames(menDF.1999)=="gun"] = "net"
  # merge data frames
  return(merge.all(menDF, menDF.1999))
}


merge.all = function(x,y){
  #
  # utility function to merge data frames
  #
  merge(x, y, all=TRUE)
}

extractResTable =
  #
  # Retrieve data from web site,
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = NULL, year = 1999, sex = "male", file = NULL)
  {
    require("XML")
    doc = htmlParse(url)

    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]
    }

    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }

selectCols = function(shortColNames, headerRow, searchLocs) {
  #
  # select columns
  #
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    startPos = regexpr(shortName, headerRow)[[1]]
    if (startPos == -1) return( c(NA, NA) )
    index = sum(startPos >= searchLocs)
    c(searchLocs[index] + 1, searchLocs[index + 1])
  }, headerRow = headerRow, searchLocs = searchLocs )
}

findColLocs = function(spacerRow) {
  #
  # find columns
  #
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)

  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

extractVariables = function(file, varNames =c("name", "home", "ag", "gun", "net", "time"))
{
  # Find the index of the row with =s
  eqIndex = grep("^===", file)
  # Extract the two key rows and the data
  spacerRow = file[eqIndex]
  headerRow = tolower(file[ eqIndex - 1 ])
  body = file[ -(1 : eqIndex) ]
       # Remove footnotes and blank rows
  footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
  if ( length(footnotes) > 0 ) body = body[ -footnotes ]
  blanks = grep("^[[:blank:]]*$", body)
  if (length(blanks) > 0 ) body = body[ -blanks ]

  # Obtain the starting and ending positions of variables
  searchLocs = findColLocs(spacerRow)
  locCols = selectCols(varNames, headerRow, searchLocs)

  Values = mapply(substr, list(body), start = locCols[1, ],
                  stop = locCols[2, ])
  colnames(Values) = varNames

  return(Values)
}

convertTime = function(time) {
  #
  # convert ##:## min:sec or ##:##:## hr:min:sec time to #.# minutes
  #
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
                      if (length(x) == 2) x[1] + x[2]/60
                      else 60*x[1] + x[2] + x[3]/60
                      })
}


createDF = function(Res, year, sex)
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']

  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])

  # Drop rows with no time
  Res = Res[ useTime != "", ]

  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']),
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}
