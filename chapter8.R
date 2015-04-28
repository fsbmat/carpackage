##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Chapter 8                               ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##

options(signif.stars=FALSE)

squareit <- function(x) {
  y <- x^2
  y
}
y <- 3
squareit(x=y)
y

inflPlot <- function(model, scale=10, col=c(1, 2),
    identify=TRUE, labels=names(rstud), ... ) {
    # Plot hatvalues, Studentized residuals, and Cook's distances
    #  for a linear or generalized linear model
    # Arguments:
    #  model: an lm or glm model object
    #  scale: a scaling factor for the circles representing Cook's D
    #  col: colors for non-noteworthy and noteworthy points
    #  identify points: label noteworthy points (TRUE or FALSE)
    #  labels: for identified points
    hatval <- hatvalues(model)
    rstud <- rstudent(model)
    cook <- sqrt(cooks.distance(model))
    scale <- scale/max(cook, na.rm = TRUE)
    p <- length(coef(model))
    n <- sum(!is.na(rstud))
    cutoff <- sqrt(4/(n - p))
    plot(hatval, rstud, xlab = "Hat-Values", ylab = "Studentized Residuals",
        cex=scale*cook, col=ifelse(cook > cutoff, col[2], col[1]), ...)
    abline(v = c(2, 3)*p/n, lty = "dashed")
    bonf <- qt(.025/n, df = n - p - 1, lower.tail=FALSE)
    abline(h=c(-bonf, -2, 0, 2, bonf), lty="dashed")
    if (identify)
    {
        noteworthy <- cook > cutoff | abs(rstud) > bonf | hatval > 2*p/n
        pos <- ifelse(hatval - mean(range(hatval, na.rm=TRUE)) <= 0, 4, 2)
        text(hatval[noteworthy], rstud[noteworthy], labels[noteworthy],
            pos = pos[noteworthy])
        return(which(noteworthy))
    }
    else return(invisible(NULL))
}

library(car)  # for Duncan data
inflPlot(lm(prestige ~ income + education, data=Duncan),
    ylim=c(-3, 4), las=1, col=gray(c(0.5, 0)))

(A <- matrix(c(1, 2, -4, 3, 5, 0), nrow=2, ncol=3))
(B <- matrix(1:6, 2, 3))
(C <- matrix(c(2, -2, 0, 1, -1, 1, 4 ,4, -4), 3, 3, byrow=TRUE))
A + B
A - B
A + C  # A and C not of the same order!
2*A

-A
A %*% C
(a <- rep(1, 3))
(b <- c(1, 5, 3))
C %*% a

a %*% C

a %*% b
outer(a, b)
t(B)
solve(C)
solve(C) %*% C  # check

library(MASS)
fractions(solve(C))
solve(C, b)
solve(C) %*% b

X <- cbind(1, as.matrix(Prestige[ , 1:3]))  # model matrix with constant
y <- Prestige[ , "prestige"]  # the response vector

head(X)  # first 6 rows
head(y)
head(Prestige[ , "prestige", drop=FALSE])
solve(t(X) %*% X) %*% t(X) %*% y
lm(prestige ~ education + income + women, data=Prestige)

R <- with(Prestige, cor(cbind(education, income, women)))
R  # correlation matrix
eigen(R)
det(R)
diag(R)  # extract diagonal

diag(R) <- NA  # set diagonal
R

diag(1:3)  # make diagonal matrix

diag(3)  # order-3 identity matrix

abs1 <- function(x) if (x < 0) -x else x
abs1(-5)
abs1(5)
abs1(-3:3)  # wrong! the first element, -3, controls the result

abs2 <- function(x) ifelse(x < 0, -x, x)
abs2(-3:3)

sign1 <- function(x) {
    if (x < 0) -1
        else if (x > 0) 1
            else 0
}
sign1(-5)
sign2 <- function(x){
    ifelse (x < 0, -1,
        ifelse(x > 0, 1, 0))
}
sign2(c(-5, 0, 10))

convert2meters <- function(x,
    units=c("inches", "feet", "yards", "miles")) {
    units <- match.arg(units)
    switch(units,
        inches = x * 0.0254,
        feet = x * 0.3048,
        yards = x * 0.9144,
        miles = x * 1609.344)
}
convert2meters(10) # uses first value of units ("inches") as default
convert2meters(10, "inches") # equivalent to default
convert2meters(3, "feet")
convert2meters(100, "y") # we can abbreviate values if unique
convert2meters(5, "miles")
convert2meters(3, "fathoms")  # produces an error!

fact1 <- function(x){
    if (x <= 1) return(1)
    f <- 1  # initialize
    for (i in 1:x) f <- f * i  # accumulate product
    f  # return result
}
fact1(5)
fact1(5.2)

fact2 <- function(x) {
    if ((!is.numeric(x)) || (x != floor(x))
         || (x < 0) || (length(x) > 1))
        stop("x must be a non-negative integer")
    if (x <= 1) return(1)
    f <- 1  # initialize
    for (i in 1:x) f <- f * i  # accumulate product
    f  # return result
}
fact2(5.2)

fact3 <- function(x){
    if ((!is.numeric(x)) || (x != floor(x))
         || (x < 0) || (length(x) > 1))
        stop("x must be a non-negative integer")
    i <- f <- 1  # initialize
    while (i <= x) {
        f <- f * i  # accumulate product
        i <- i + 1  # increment counter
        }
    f  # return result
}
fact3(5)

fact4 <- function(x) {
    if ((!is.numeric(x)) || (x != floor(x))
         || (x < 0) || (length(x) > 1))
        stop("x must be a non-negative integer")
    i <- f <- 1  # initialize
    repeat {
        f <- f * i  # accumulate product
        i <- i + 1  # increment counter
        if (i > x) break  # termination test
    }
    f  # return result
}
fact4(5)

fact5 <- function(x){
    if (x <= 1) 1  # termination condition
    else x * fact5(x - 1)  # recursive call
}
fact5(5)
trace(fact5)
fact5(5)
untrace(fact5)

fact6 <- fact5
remove(fact5)
fact6(5)  # tries to call the removed fact5

fact7 <- function(x) {
    if (x <= 1) 1
    else x * Recall(x - 1)  # recursive call
}
fact7(5)
fact8 <- fact7
remove(fact7)
fact8(5)  # still works with fact7 removed

head(DavisThin, 10)  # first 10 rows
dim(DavisThin)
DavisThin$thin.drive <- apply(DavisThin, 1, sum)
head(DavisThin$thin.drive, 10)
apply(DavisThin, 2, mean)
colMeans(DavisThin)
DavisThin$thin.drive <- NULL  # remove thin.drive
DavisThin[1, 2] <- DavisThin[2, 4] <- DavisThin[10, 3] <- NA
head(DavisThin, 10)
head(apply(DavisThin, 1, sum), 10)
head(apply(DavisThin, 1, function(x) 7*mean(x, na.rm=TRUE)), 10)
DavisThin[1, 2:5] <- NA  # create more missing data
head(DavisThin, 10)

makeScale <- function(items) {
    if (sum(is.na(items)) >= 4) NA
    else 7*mean(items, na.rm=TRUE)
}
head(apply(DavisThin, 1, makeScale), 10)

thin.list <- as.list(DavisThin)
str(thin.list)  # structure of the result
lapply(thin.list, mean, na.rm=TRUE)
lapply(thin.list, function(x) mean(x, na.rm=TRUE))
sapply(thin.list, mean, na.rm=TRUE)

(result <- integrate(dnorm, lower=-1.96, upper=1.96))
names(result)
(low <- c(-Inf, -3:3))
(high <- c(-3:3, Inf))
(P <- mapply(function(lo, hi) integrate(dnorm, lo, hi)$value,
    lo=low, hi=high))
sum(P)
pnorm(high) - pnorm(low)

Integrate <- Vectorize(
    function(fn, lower, upper) integrate(fn, lower, upper)$value,
    vectorize.args=c("lower", "upper")
    )
Integrate(dnorm, lower=low, upper=high)

set.seed(12345) # to reproduce results in the text
some(Moore)  # randomly sample 10 observations
Moore$fcategory <- factor(Moore$fcategory,
    levels=c("high", "medium", "low"))
with(Moore, tapply(conformity,
   list(Status=partner.status, Authoritarianism=fcategory), mean))
   
time1 <- function(n) {  # inefficient!
    a <- NULL
    for (i in 1:n) a <- c(a, i^2)
    a
}
system.time(time1(30000))
time2 <- function(n) {  # better
    a <- numeric(n)
    for (i in 1:n) a[i] <- i^2
    a
}
system.time(time2(30000))
time3 <- function(n) {  # best
    a <- (1:n)^2
    a
}
system.time(time3(30000))

system.time({
     matrices <- vector(mode="list", length=10000)
     for (i in 1:10000) matrices[[i]] <- matrix(rnorm(10000), 100, 100)
 })
system.time({  # (slightly) inefficient
     matrices <- list()
     for (i in 1:10000) matrices[[i]] <- matrix(rnorm(10000), 100, 100)
 })
system.time({  # inefficient!
     matrices <- list()
     for (i in 1:1000) matrices <-
         c(matrices, list(matrix(rnorm(10000), 100, 100)))
 })

time4 <- function(n) {  # (slightly) inefficient!
    a <- numeric(n)
    for (i in 1:n) a[i] <- 2 * pi * sin(i)
    a
}
system.time(time4(100000))
time5 <- function(n) {  # better
    a <- numeric(n)
    for (i in 1:n)
        a[i] <- sin(i)
    2 * pi * a
}
system.time(time5(100000))

system.time({
    S <- matrix(0, 100, 100)
    for (M in matrices) S <- S + M
})
# opaque and wastes memory!
system.time(S <- apply(array(unlist(matrices),
    dim = c(100, 100, 10000)), 1:2, sum))
# opaque & wastes memory!
S <- rowSums(array(unlist(matrices),
    dim = c(10, 10, 10000)), dims = 2)

lreg1 <- function(X, y, max.iter=10, tol=1E-6, verbose=FALSE){
    # X is the model matrix
    # y is the response vector of 0s and 1s
    # max.iter is the maximum number of iterations
    # tol is a convergence criterion
    # verbose: show iteration history?
    X <- cbind(1, X)  # add constant
    b <- b.last <- rep(0, ncol(X))  # initialize coefficients
    it <- 1  # initialize iteration counter
    while (it <= max.iter){
        if (verbose) cat("\niteration = ", it, ": ", b)
        p <- as.vector(1/(1 + exp(-X %*% b)))
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)  # update coefficients
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b  # update previous coefficients
        it <- it + 1  # increment counter
    }
    if (verbose) cat("\n")  # newline
    if (it > max.iter) warning("maximum iterations exceeded")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

head(Mroz)  # first 6 observations
Mroz$lfp <- with(Mroz, ifelse(lfp == "yes", 1, 0))
Mroz$wc <- with(Mroz, ifelse(wc == "yes", 1, 0))
Mroz$hc <- with(Mroz, ifelse(hc == "yes", 1, 0))
mod.mroz.1 <- with(Mroz, lreg1(cbind(k5, k618, age, wc, hc, lwg,
    inc), lfp, verbose=TRUE))
out <- with(mod.mroz.1,
    cbind(Estimate=coefficients, "Std. Error"=sqrt(diag(var))))
rownames(out)[1] <- "Intercept"
out

lreg2 <- function(X, y, method="BFGS") {
    X <- cbind(1, X)
    negLogL <- function(b, X, y){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        - sum(y*log(p) + (1 - y)*log(1 - p))
    }
    grad <- function(b, X, y){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        - colSums((y - p)*X)
    }
    result <- optim(rep(0, ncol(X)), negLogL, gr=grad,
        hessian=TRUE, method=method, X=X, y=y)
    list(coefficients=result$par, var=solve(result$hessian),
        deviance=2*result$value, converged=result$convergence == 0)
}

mod.mroz.2 <- with(Mroz, lreg2(cbind(k5, k618, age, wc, hc, lwg,
     inc), lfp))
mod.mroz.2$coefficients
sqrt(diag(mod.mroz.2$var))
mod.mroz.2$converged

Mroz$LFP <- ifelse(Mroz$lfp==0, "no", "yes")
mod.mroz.2a <- with(Mroz, lreg2(cbind(k5, k618, age, wc, hc, lwg,
     inc), LFP))

makeDigits <- function(x) strsplit(as.character(x), "")[[1]]
makeDigits(123456)
makeDigits(-123456)
makeDigits(1000000000)
options(scipen=100)
makeDigits(1000000000)

makeNumber <- function(x) as.numeric(paste(x, collapse=""))
makeNumber(c("1", "2", "3", "4", "5"))

ones <- c("zero", "one", "two", "three", "four", "five", "six",
    "seven", "eight", "nine")
teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen",
    "fifteen", "sixteen", " seventeen", "eighteen", "nineteen")
names(ones) <- names(teens) <- 0:9
tens <- c("twenty", "thirty", "forty", "fifty", "sixty",
    "seventy", "eighty", "ninety")
names(tens) <- 2:9
suffixes <- c("thousand,", "million,", "billion,", "trillion,")
ones["5"]
teens["3"]
tens["7"]

trim <- function(text){
    gsub("(^\ *)|((\ *|-|,\ zero|-zero)$)", "", text)
}
number2words <- function(x){
   negative <- x < 0
   x <- abs(x)
     digits <- makeDigits(x)
      nDigits <- length(digits)
      result <- if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
          if (x <= 19) as.vector(teens[digits[2]])
            else trim(paste(tens[digits[1]], "-", ones[digits[2]], sep=""))
     else if (nDigits == 3) {
          tail <- makeNumber(digits[2:3])
          if (tail == 0) paste(ones[digits[1]], "hundred")
          else trim(paste(ones[digits[1]], "hundred", number2words(tail)))
      }
      else {
          nSuffix <- ((nDigits + 2) %/% 3) - 1
          if (nSuffix > length(suffixes) || nDigits > 15)
              stop(paste(x, "is too large!"))
          pick <- 1:(nDigits - 3*nSuffix)
          trim(paste(number2words(makeNumber(digits[pick])),
              suffixes[nSuffix], number2words(makeNumber(digits[-pick]))))
      }
      if (negative) paste("minus", result) else result
  }

number2words(123456789)
number2words(-123456789)
number2words(-123456000)

options(scipen=0)

numbers2words <- function(x, billion=c("US", "UK"),
    and=if (billion == "US") "" else "and") {
    billion <- match.arg(billion)
    trim <- function(text) {
        gsub("(^\ *)|((\ *|-|,\ zero|-zero)$)", "", text)
    }
    makeNumber <- function(x) as.numeric(paste(x, collapse=""))
    makeDigits <- function(x) strsplit(as.character(x), "")[[1]]
    helper <- function(x) {
        negative <- x < 0
        x <- abs(x)
        digits <- makeDigits(x)
        nDigits <- length(digits)
        result <- if (nDigits == 1) as.vector(ones[digits])
        else if (nDigits == 2)
            if (x <= 19) as.vector(teens[digits[2]])
                else trim(paste(tens[digits[1]], "-", ones[digits[2]], sep=""))
        else if (nDigits == 3) {
            tail <- makeNumber(digits[2:3])
            if (tail == 0) paste(ones[digits[1]], "hundred")
                else trim(paste(ones[digits[1]], trim(paste("hundred", and)),
                    helper(tail)))
        }
        else {
            nSuffix <- ((nDigits + 2) %/% 3) - 1
            if (nSuffix > length(suffixes) || nDigits > 15)
                stop(paste(x, "is too large!"))
            pick <- 1:(nDigits - 3*nSuffix)
            trim(paste(helper(makeNumber(digits[pick])),
                suffixes[nSuffix], helper(makeNumber(digits[-pick]))))
        }
        if (billion == "UK"){
            words <- strsplit(result, " ")[[1]]
            if (length(grep("million,", words)) > 1)
                result <- sub(" million, ", ", ", result)
        }
        if (negative) paste("minus", result) else result
    }
    opts <- options(scipen=100)
    on.exit(options(opts))
    ones <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
        "eight", "nine")
    teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
        "sixteen", " seventeen", "eighteen", "nineteen")
    names(ones) <- names(teens) <- 0:9
    tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
        "ninety")
    names(tens) <- 2:9
    suffixes <- if (billion == "US")
                    c("thousand,", "million,", "billion,", "trillion,")
                else
                    c("thousand,", "million,", "thousand million,", "billion,")
    x <- round(x)
    if (length(x) > 1) sapply(x, helper) else helper(x)
}

numbers2words(c(1234567890123, -0123, 1000))
numbers2words(c(1234567890123, -0123, 1000), billion="UK")
numbers2words(c(1234567890123, -0123, 1000), and="and")

# bugged!
lreg3 <- function(X, y, max.iter=10, tol=1E-6, verbose=FALSE) {
    X <- cbind(1, X)
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        if (verbose) cat("\niteration = ", it, ": ", b)
        p <- 1/(1 + exp(-X %*% b))
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    if (verbose) cat("\n")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

mod.mroz.1b <- with(Mroz,
    lreg3(cbind(k5, k618, age, wc, hc, lwg, inc), lfp))
traceback()

lreg3 <- function(X, y, max.iter=10, tol=1E-6, verbose=FALSE) {
    X <- cbind(1, X)
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        if (verbose) cat("\niteration = ", it, ": ", b)
        p <- 1/(1 + exp(-X %*% b))
        V <- diag(p * (1 - p))
      browser()
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
      }
    if (it > max.iter) warning("maximum iterations exceeded")
    if (verbose) cat("\n")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

mod.mroz.1b <- with(Mroz,
     lreg3(cbind(k5, k618, age, wc, hc, lwg, inc), lfp))
objects() # in local environment of lreg3
str(X)
str(V)
str(p)
str(p * (1 - p))
Q

# restore original bugged function
lreg3 <- function(X, y, max.iter=10, tol=1E-6, verbose=FALSE) {
    X <- cbind(1, X)
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        if (verbose) cat("\niteration = ", it, ": ", b)
        p <- 1/(1 + exp(-X %*% b))
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    if (verbose) cat("\n")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

options(error=dump.frames)
mod.mroz.1b <- with(Mroz,
    lreg3(cbind(k5, k618, age, wc, hc, lwg, inc), lfp))
debugger()
5
str(X)
str(V)
Q
options(error=NULL)

set.seed(12345)  # for reproducibility
X <- matrix(rnorm(5000*10), 5000, 10)
y <- rbinom(5000, 1, prob=1/(1 + exp(- cbind(1, X) %*% rep(1, 11))))

system.time(mod.1 <- lreg1(X, y))
mod.1$coef
system.time(mod.2 <- lreg2(X, y))
mod.2$coef
system.time(mod.glm <- glm(y ~ X, family=binomial))
coef(mod.glm)

(tmp <- tempfile()) # create temporary file
Rprof(tmp, memory.profiling=TRUE) # turn on profiling
mod.1 <- lreg1(X, y)
Rprof() # turn off profiling
summaryRprof(tmp, memory="both") # summarize results
unlink(tmp) # delete temporary file

lreg4 <- function(X, y, max.iter=10, tol=1E-6) {
    X <- cbind(1, X)
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        var.b <- solve(crossprod(X, p * (1 - p) * X))
        b <- b + var.b %*% crossprod(X, y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

system.time(mod.1c <- lreg4(X, y))
mod.1c$coef

tmp <- tempfile()
Rprof(tmp, memory.profiling=TRUE, interval=0.002)
mod.1c <- lreg4(X, y)
Rprof()
summaryRprof(tmp, memory="both")$by.total
unlink(tmp)

summary
print

lreg <- function(X, ...){
    UseMethod("lreg")
    }

lreg.default <- function(X, y, predictors=colnames(X), max.iter=10,
        tol=1E-6, constant=TRUE, ...) {
    if (!is.numeric(X) || !is.matrix(X))
        stop("X must be a numeric matrix")
    if (!is.numeric(y) || !all(y == 0 | y == 1))
        stop("y must contain only 0s and 1s")
    if (nrow(X) != length(y))
        stop("X and y contain different numbers of observations")
    if (constant) {
        X <- cbind(1, X)
        colnames(X)[1] <- "Constant"
    }
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        var.b <- solve(crossprod(X, p * (1 - p) * X))
        b <- b + var.b %*% crossprod(X, y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    dev <- -2*sum(y*log(p) + (1 - y)*log(1 - p))
    result <- list(coefficients=as.vector(b), var=var.b,
        deviance=dev, converged= it <= max.iter,
        predictors=predictors, iterations = it)
    class(result) <- "lreg"
    result
}

mod.mroz.3 <- with(Mroz, lreg(cbind(k5, k618, age, wc, hc, lwg, inc), lfp))
class(mod.mroz.3)
str(mod.mroz.3) # to avoid printing the entire object

print.lreg <- function(x, ...) {
    coef <- x$coefficients
    names(coef) <- x$predictors
    print(coef)
    invisible(x)
}

summary.lreg <- function(object, ...) {
    b <- object$coefficients
    se <- sqrt(diag(object$var))
    z <- b/se
    table <- cbind(b, se, z, 2*(1-pnorm(abs(z))))
    colnames(table) <- c("Estimate", "Std.Err", "z value", "Pr(>|z|)")
    rownames(table) <- object$predictors
    result <- list(coef=table, deviance=object$deviance,
        converged=object$converged)
    class(result) <- "summary.lreg"
    result
}

print.summary.lreg <- function(x, ...) {
    printCoefmat(x$coef, signif.stars=FALSE)
    cat("\nDeviance =", x$deviance,"\n")
    if (!x$converged) cat("\n Note: lreg did not converge\n")
    invisible(x)
}

mod.mroz.3
summary(mod.mroz.3)

setClass("lreg5",
    representation(coefficients="numeric", var="matrix",
        deviance="numeric", predictors="character",
        iterations="numeric"))

lreg5 <- function(X, y, predictors=colnames(X), max.iter=10,
        tol=1E-6, constant=TRUE, ...) {
    if (!is.numeric(X) || !is.matrix(X))
        stop("X must be a numeric matrix")
    if (!is.numeric(y) || !all(y == 0 | y == 1))
        stop("y must contain only 0s and 1s")
    if (nrow(X) != length(y))
        stop("X and y contain different numbers of observations")
    if (constant) {
        X <- cbind(1, X)
        colnames(X)[1] <- "Constant"
    }
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        var.b <- solve(crossprod(X, p * (1 - p) * X))
        b <- b + var.b %*% crossprod(X, y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    dev <- -2*sum(y*log(p) + (1 - y)*log(1 - p))
    result <- new("lreg5", coefficients=as.vector(b), var=var.b,
        deviance=dev, predictors=predictors, iterations=it)
    result
}

mod.mroz.4 <- with(Mroz,
    lreg5(cbind(k5, k618, age, wc, hc, lwg, inc), lfp))
class(mod.mroz.4)
slotNames(mod.mroz.4)
str(mod.mroz.4)

show
setMethod("show", signature(object="lreg5"),
    definition=function(object) {
            coef <- object@coefficients
            names(coef) <- object@predictors
            print(coef)
        }
    )
    
mod.mroz.4

setMethod("summary", signature(object="lreg5"),
    definition=function(object, ...) {
            b <- object@coefficients
            se <- sqrt(diag(object@var))
            z <- b/se
            table <- cbind(b, se, z, 2*(1-pnorm(abs(z))))
            colnames(table) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
            rownames(table) <- object@predictors
            printCoefmat(table)
            cat("\nDeviance =", object@deviance,"\n")
        }
    )
    
summary(mod.mroz.4)

lreg.formula <- function(formula, data, subset, na.action, model = TRUE,
    contrasts = NULL, ...) {
    call <- match.call()  # returns the function call
    mf <- match.call(expand.dots = FALSE)  # the function call w/o ...
    args <- match(c("formula", "data", "subset", "na.action"),
        names(mf), 0)  # which arguments are present?
    mf <- mf[c(1, args)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval.parent(mf)  # create a model frame
    terms <- attr(mf, "terms")  # terms object for the model
    y <- model.response(mf)  # response variable
    values <- sort(unique(y))
    if (length(values) != 2) stop("the response variable is not binary")
    y <- as.numeric(y == values[2])  # higher value coded 1, lower coded 0
    X <- model.matrix(terms, mf, contrasts)  # model matrix
    mod <- lreg.default(X, y, predictors=colnames(X), constant=FALSE, ...)
    mod$na.action <- attr(mf, "na.action")
    mod$contrasts <- attr(X, "contrasts")
    mod$xlevels <- .getXlevels(terms, mf)
    mod$call <- call
    mod$terms <- terms
    if (model)  mod$model <- mf
    mod
}

vcov.lreg <- function(object, ...) {
    object$var
}

remove(Mroz) # delete modified Mroz data set
mod.lreg <- lreg(lfp ~ ., data=Mroz) # Mroz will be found in car
mod.lreg
str(vcov(mod.lreg))  # i.e., an 8 x 8 matrix
coef(mod.lreg) # inherits coef.default

Glmnet <- function(formula, data, subset, na.action, ...) {
    call <- match.call()  # returns the function call
    mf <- match.call(expand.dots = FALSE)  # the function call w/o ...
    args <- match(c("formula", "data", "subset", "na.action"),
        names(mf), 0)  # which arguments are present?
    mf <- mf[c(1, args)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval.parent(mf)  # create a model frame
    terms <- attr(mf, "terms")  # terms object for the model
    y <- model.response(mf)  # response variable
    X <- model.matrix(terms, mf, contrasts)  # model matrix
    glmnet(X,y,...)
}

library(glmnet)
args(glmnet)
g1 <- Glmnet(prestige ~ income + education + women + type,
    data=Prestige)
class(g1)

f <- function(x) x + a
a <- 10
x <- 5
f(2)

f <- function(x) {
    a <- 5
    g(x)
}
g <- function(y) y + a
f(2)

f <- function(x) {
    a <- 5
    g <- function(y) y + a
    g(x)
}
f(2)

makePower <- function(power) {
    function(x) x^power
}

square <- makePower(2)
square
square(4)

cuberoot <- makePower(1/3)
cuberoot
cuberoot(64)

m1 <- lm(prestige ~ income + education + women + type,
    data=na.omit(Prestige))
yhat2 <- predict(m1)^2
m2 <- update(m1, . ~ . + yhat2)
test <- summary(m2)$coef["yhat2", 3]
pval <- 2*pnorm(abs(test), lower.tail=FALSE)
c(TukeyTest=test, Pvalue=pval)

tukeyTest <- function(model) {  # bugged!
   yhat2 <- predict(model)^2
   m2 <- update(model, . ~ . + yhat2)
   test <- summary(m2)$coef["yhat2", 3]
   pval <- 2*pnorm(abs(test), lower.tail=FALSE)
   c(TukeyTest=test, Pvalue=pval)
}
remove(yhat2)
tukeyTest(m1)

tukeyTest <- function(model) {
    yhat2 <<- predict(model)^2  # note the use of "<<-"
    m2 <- update(model, . ~ . + yhat2)
    test <- summary(m2)$coef["yhat2", 3]
    pval <- 2*pnorm(abs(test), lower.tail=FALSE)
    rm(yhat2, envir=globalenv())
    c(TukeyTest=test, Pvalue=pval)
}
tukeyTest(m1)

getAnywhere(tukeyNonaddTest)