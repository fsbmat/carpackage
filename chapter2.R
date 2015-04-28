##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Chapter 2                               ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##


# turn off significance stars in lm output
options(show.signif.stars=FALSE)

(x <- c(1, 2, 3, 4))
(names <- c("John", "Sandy", 'Mary'))
(v <- c(TRUE, FALSE))

cooperation <- c(49,64,37,52,68,54,61,79,64,29,27,58,52,41,30,40,39,44,34,44)

rep(5, 3)
rep(c(1, 2, 3), 2)
rep(1:3, 3:1)
(condition <- rep(c("public", "anonymous"), c(10, 10)))
(sex <- rep(rep(c("male", "female"), c(5, 5)), 2))

(Guyer <- data.frame(cooperation, condition, sex))

library(car)
Prestige
(Prestige <- read.table(file.choose(), header=TRUE))
Prestige <- read.table(
   "http://socserv.socsci.mcmaster.ca/jfox/books/Companion/data/Prestige.txt",
   header=TRUE)
summary(Prestige)

file <-
"http://socserv.socsci.mcmaster.ca/jfox/books/Companion/data/Prestige-bugged.txt"
Prestige <- read.table(file, header=TRUE)
(counts <- count.fields(file))
which(counts != 7)

file <-
"http://socserv.socsci.mcmaster.ca/jfox/books/Companion/data/Prestige-fixed.txt"
Prestige <- read.fwf(file,
    col.names=c("occupation", "education", "income", "women",
        "prestige", "census", "type"),
    row.names="occupation",
    widths=c(25, 5, 5, 5, 4, 4, 4))

# The following example works on Windows
library(RODBC)
channel <- odbcConnectExcel("D:/data/Datasets.xls") # use location on your file system