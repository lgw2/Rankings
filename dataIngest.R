collegeMens <- read.table("/Users/luciawilliams/Downloads/OnlyWinsRatePer10.csv", header=TRUE, sep=",")

per1 <- data.frame(subset(collegeMens, RatePer == 1))
per2 <- data.frame(subset(collegeMens, RatePer == 2))
per3 <- data.frame(subset(collegeMens, RatePer == 3))
per4 <- data.frame(subset(collegeMens, RatePer == 4))
per5 <- data.frame(subset(collegeMens, RatePer == 5))
per6 <- data.frame(subset(collegeMens, RatePer == 6))
per7 <- data.frame(subset(collegeMens, RatePer == 7))
per8 <- data.frame(subset(collegeMens, RatePer == 8))
per9 <- data.frame(subset(collegeMens, RatePer == 9))
per10 <- data.frame(subset(collegeMens, RatePer == 10))
per11 <- data.frame(subset(collegeMens, RatePer == 11))
per12 <- data.frame(subset(collegeMens, RatePer == 12))
per13 <- data.frame(subset(collegeMens, RatePer == 13))
per14 <- data.frame(subset(collegeMens, RatePer == 14))
per15 <- data.frame(subset(collegeMens, RatePer == 15))
per16 <- data.frame(subset(collegeMens, RatePer == 16))
per17 <- data.frame(subset(collegeMens, RatePer == 17))
per18 <- data.frame(subset(collegeMens, RatePer == 18))
per19 <- data.frame(subset(collegeMens, RatePer == 19))
per20 <- data.frame(subset(collegeMens, RatePer == 20))

uniquePeriods <- unique(collegeMens$RatePer)

library(XML)
library(stringr)

# USAU page
url <- "http://www.usaultimate.org/archives/2011_college.aspx"

# Open a browser to see the web page
browseURL(url)

# Extract HTML tables as dataframes
listOfTables <- readHTMLTable(url, stringsAsFactors=FALSE)
# this might have been the wrong thing to do. but i think i've got what I wanted...
table38 <- listOfTables[38]
finalRankingsDF <- table38[[1]]
finalRankings2011 <- finalRankingsDF[4:303,]