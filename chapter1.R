##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Chapter 1                               ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##


2 + 3 # addition
2 - 3 # subtraction
2*3   # multiplication
2/3   # division
2^3   # exponentiation
4^2 - 3*2
1 - 6 + 4
(4^2) - (3*2)
(4 + 3)^2
4 + 3^2
-2--3
-2 - -3
log(100)
log(100, base=10)
log10(100) # equivalent
log(100, b=10)

help(log)

example("log")

args(log)
log(100, 10)
`+`(2, 3)
c(1, 2, 3, 4)
1:4 # integer sequence
4:1
-1:2
seq(1, 4)
seq(2, 8, by=2) # specify interval
seq(0, 1, by=0.1) # non-integer sequence
seq(0, 1, length=11) # specify number of elements
c(1, 2, 3, 4)/2
c(1, 2, 3, 4)/c(4, 3, 2, 1)
log(c(0.1, 1, 10, 100), 10)
c(1, 2, 3, 4) + c(4, 3) # no warning
c(1, 2, 3, 4) + c(4, 3, 2) # produces warning
x <- c(1, 2, 3, 4) # assignment
x # print
x/2
(y <- sqrt(x))

### set random number seed to agree with text.
set.seed(12345)

x <- rnorm(100)
summary(x)
(words <- c("To", "be", "or", "not", "to", "be"))
paste(words, collapse=" ")
(vals <- c(TRUE, TRUE, FALSE, TRUE))
!vals
sum(vals)
sum(!vals)
c("A", FALSE, 3.0)
x[12]    # 12th element
words[2] # second element
vals[3]     # third element
x[6:15] # elements 6 through 15
x[-(11:100)] # omit elements 11 through 100
1 == 2
1 != 2
1 <= 2
1 < 1:3
3:1 > 1:3
3:1 >= 1:3
TRUE & c(TRUE, FALSE)
c(TRUE, FALSE, FALSE) | c(TRUE, TRUE, FALSE)
(z <- x[1:10])
z < -0.5
z > 0.5
z < -0.5 | z > 0.5  #  < and > of higher precedence than |
abs(z) > 0.5  # absolute value
z[abs(z) > 0.5]
z[!(abs(z) > 0.5)]
mean(x)
sum(x)/length(x)
myMean <- function(x) sum(x)/length(x)
myMean(x)
y # from sqrt(c(1, 2, 3, 4))
myMean(y)
myMean(1:100)
myMean(sqrt(1:100))
myVar <- function(x) sum((x - myMean(x))^2)/(length(x) - 1)
myVar(1:100)
var(1:100) # check
letters

myVar(letters)  # produces an error
traceback()
apropos("log")
objects()
remove(x, y, z, vals, words)

Duncan <- read.table(file.choose(), header=TRUE)
# alternatively, you can get the data from the car package
# library(car)
# data(Duncan)

summary(Duncan)
attach(Duncan)
prestige
hist(prestige)
pairs(cbind(prestige, income, education),
    panel=function(x, y){
        points(x, y)
        abline(lm(y ~ x), lty="dashed")
        lines(lowess(x, y))
    },
    diag.panel=function(x){
        par(new=TRUE)
        hist(x, main="", axes=FALSE)
    }
)

pairs(cbind(prestige, income, education))

pairs(Duncan[,-1])

scatmat <- function(...) {
    pairs(cbind(...),
        panel=function(x, y){
            points(x, y)
            abline(lm(y ~ x), lty=2)
            lines(lowess(x, y))
        },
        diag.panel=function(x){
            par(new=TRUE)
            hist(x, main="", axes=FALSE)
        }
    )
}

scatmat(prestige, income, education)

plot(income, education)
#   Use the mouse to identify points 
#   (to exit: right-click, Windows; esc, Mac OS X):
identify(income, education, row.names(Duncan))

row.names(Duncan)[c(6, 16, 27)]
(duncan.model <- lm(prestige ~ income + education))
summary(duncan.model)
options(show.signif.stars=FALSE)


library(car)

hist(rstudent(duncan.model))

# reset random number seed to agree with the text
set.seed(12345)

qqPlot(duncan.model, labels=row.names(Duncan), id.n=3)
influenceIndexPlot(duncan.model, vars=c("Cook", "hat"), id.n=3)
avPlots(duncan.model, id.n=3, id.cex=0.75)
crPlots(duncan.model, span=0.7)
spreadLevelPlot(duncan.model)
ncvTest(duncan.model)
ncvTest(duncan.model, var.formula= ~ income + education)
(remove <- whichNames(c("minister", "conductor"), Duncan))
summary(update(duncan.model, subset=-remove))
summary(Duncan$type)
summary(Duncan$prestige)
summary(Duncan)
summary(lm(prestige ~ income + education, data=Duncan))
class(Duncan$type)
class(Duncan$prestige)
class(Duncan)
duncan.model <- lm(prestige ~ income + education)
class(duncan.model)
summary

args(summary.lm)

methods(summary)

mod.mroz <- glm(lfp ~ ., family=binomial, data=Mroz)
class(mod.mroz)