##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Chapter 7                               ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##

options(show.signif.stars=FALSE)

args(plot.default)

plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
par("col")
names(par())

plot(1:25, pch=1:25, xlab="Symbol Number", ylab="")
lines(1:25, type="h", lty="dashed")

head(letters) # first 6 lowercase letters
plot(1:26, xlab="letters", ylab="", pch=letters,
    axes=FALSE, frame.plot=TRUE)
    
plot(c(1, 7), c(0, 1), type="n", axes=FALSE,
    xlab="Line Type (lty)", ylab="", frame.plot=TRUE)
axis(1, at=1:6)  # x-axis
for (lty in 1:6)
    lines(c(lty, lty, lty + 1), c(0, 0.5, 1), lty=lty)
    
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
abline(0, 1)
abline(c(1, -1), lty="dashed")
abline(h=seq(0, 1, by=0.1), v=seq(0, 1, by=0.1), col="gray")

library(car)  # for data
plot(prestige ~ income, type="n", data=Prestige)
grid(lty="solid")
with(Prestige, points(income, prestige, pch=16, cex=1.5))

par(mfrow=c(1, 2))
plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="",
    frame.plot=TRUE, main="(a)")
text(x=c(0.2, 0.5), y=c(0.2, 0.7),
    c("example text", "another string"))
plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="",
    frame.plot=TRUE, main="(b)")
# to exit locator, right-click on Windows, esc on Mac OS X
text(locator(), c("one", "two", "three"))

plot(c(1, 5), c(0, 1), axes=FALSE, type="n",
    xlab="", ylab="", main="(a) arrows")
arrows(x0=1:5, y0=rep(0.1, 5),
    x1=1:5, y1=seq(0.3, 0.9, len=5), code=3)

plot(c(1, 5), c(0, 1), axes=FALSE, type="n",
    xlab="", ylab="", main="(b) segments")
segments(x0=1:5, y0=rep(0.1, 5),
    x1=1:5, y1=seq(0.3, 0.9, len=5))
par(mfrow=c(1, 1))

plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
polygon(c(0.2, 0.8, 0.8), c(0.2, 0.2, 0.8), col="black")
polygon(c(0.2, 0.2, 0.8), c(0.2, 0.8, 0.8))

plot(c(1, 5), c(0, 1), axes=FALSE, type="n",
    xlab="", ylab="", frame.plot=TRUE)
# click once to locate legend
legend(locator(1), legend=c("group A", "group B", "group C"),
    lty=c(1, 2, 4), pch=1:3)

par(mfrow=c(1, 2))
curve(x*cos(25/x), 0.01, pi, n=1000)
curve(sin, 0, 2*pi, ann=FALSE, axes=FALSE, lwd=2)
axis(1, pos=0, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
    labels=c(0, expression(pi/2), expression(pi),
        expression(3*pi/2), expression(2*pi)))
axis(2, pos=0)
curve(cos, add=TRUE, lty="dashed", lwd=2)
legend(pi, 1, lty=1:2, lwd=2, legend=c("sine", "cosine"), bty="n")
par(mfrow=c(1, 1))

rainbow(10)
gray(0:9/9)
colors()[1:10]

palette()
pie(rep(1, 8), col=1:8)

library(colorspace)
pie(rep(1, 100), col=rainbow(100), labels=rep("", 100))
pie(rep(1, 100), col=rainbow_hcl(100), labels=rep("", 100))
pie(rep(1, 100), col=gray(0:100/100), labels=rep("", 100))

oldpar <- par(mfrow=c(2, 2), las=1)   # 2 x 2 array of graphs

UN <- na.omit(UN)
gdp <- UN$gdp
infant <- UN$infant.mortality
ord <- order(gdp)   # sort data by gdp
gdp <- gdp[ord]
infant <- infant[ord]

x0 <- gdp[150]           # focal x = x_(150)
dist <- abs(gdp - x0)   # distance from focal x
h <- sort(dist)[95]     # bandwidth for span of .5 (where n = 190)
pick <- dist <= h       # observations within window

plot(gdp, infant, xlab="GDP per Capita", ylab="Infant-Mortality Rate",
    type="n", main="(a) Observations Within the Window\nspan = 0.5")
points(gdp[pick], infant[pick], col="black")
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0)    # focal x
abline(v=c(x0 - h, x0 + h), lty=2)  # window
#text(locator(1), expression(x[(150)]), xpd=TRUE)
text(x0, par("usr")[4] + 10, expression(x[(150)]), xpd=TRUE)

plot(range(gdp), c(0,1), xlab="GDP per Capita",
    ylab="Tricube Kernel Weight",
    type="n", main="(b) Tricube Weights")
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)

tricube <- function(x, x0, h) {
    z <- abs(x - x0)/h
    ifelse(z < 1, (1 - z^3)^3, 0)
}
tc <- function(x) tricube(x, x0, h) # to use with curve
curve(tc, min(gdp), max(gdp), n=1000, lwd=2, add=TRUE)
points(gdp[pick], tricube(gdp, x0, h)[pick], col="gray20")
abline(h=c(0, 1), col="gray")

plot(gdp, infant, xlab="GDP per Capita", ylab="Infant-Mortality Rate",
    type="n", main="(c) Weighted Average (Kernal Estimate)")
points(gdp[pick], infant[pick], col="black")
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)
yhat <- weighted.mean(infant, w=tricube(gdp,  x0, h))  # kernel estimate
lines(c(x0 - h, x0 + h), c(yhat, yhat), lwd=3)

plot(gdp, infant, xlab="GDP per Capita", ylab="Infant-Mortality Rate",
    main="(d) Complete Kernel Estimate")
yhat <- numeric(length(gdp))
for (i in 1:length(gdp)){   # kernel estimate at each x
    x0 <- gdp[i]
    dist <- abs(gdp - x0)
    h <- sort(dist)[95]
    yhat[i] <- weighted.mean(infant, w=tricube(gdp, x0, h))
    }
lines(gdp, yhat, lwd=2)
par(oldpar)

set.seed(12345) # to reproduce results in the text
par(oma=c(0, 0, 1, 0), mar=c(2, 3, 3, 2)) # room in top outer margin
par(fig=c(0, 0.5, 0.5, 1)) # top-left panel
x <- seq(0, 1, length=200)
Ey <- rev(1 - x^2)  # expected value of y; reverse values
y <- Ey + 0.1*rnorm(200)  # add errors
plot(x, y, axes=FALSE, frame=TRUE, main="(a) monotone, simple",
    cex.main=1, xlab="", ylab="", col="gray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)  # text in bottom margin
mtext("y ", side=2, at=max(y), las=1)  # text in left margin

par(fig=c(0.5, 1, 0.5, 1)) # top-right panel
par(new=TRUE)
x <- seq(0.02, 0.99, length=200)
Ey <- log(x/(1 - x))
y <- Ey + 0.5*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(b) monotone, not simple",
    cex.main=1, xlab="", ylab="", col="gray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

par(fig=c(0.25, 0.75, 0, 0.5)) # bottom panel
par(new=TRUE)
x <- seq(0.2, 1, length=200)
Ey <- (x - 0.5)^2
y <- Ey + 0.04*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(c) non-monotone, simple",
    cex.main=1, xlab="", ylab="", col="gray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)
title("Nonlinear Relationships", outer=TRUE)

library(lattice)
xyplot(salary ~ yrs.since.phd | discipline:rank, groups=sex,
   data=Salaries, type=c("g", "p", "r"), auto.key=TRUE)

library(latticeExtra)
useOuterStrips(
  bwplot(salary ~ sex | rank + discipline, data=Salaries,
    scales=list(x=list(rot=45), y=list(log=10, rot=0) )),
  strip.left=strip.custom(strip.names=TRUE, var.name="Discipline"))

head(Depredations)
library(maps)
par(mfrow=c(1, 2))
map("county", "minnesota", col=gray(0.4))
with(Depredations, points(longitude, latitude,
     cex=sqrt(early), pch=20))
title("Depredations, 1976-1991", cex.main=1.5)
map("county", "minnesota", col=grey(0.4))
with(Depredations,points(longitude, latitude,
     cex=sqrt(late), pch=20))
title("Depredations, 1992-1998", cex.main=1.5)
par(mfrow=c(1, 1))

library(ggplot2)
qplot(income, prestige, xlab="Average Income", ylab="Prestige Score",
    geom=c("point", "smooth"), data=Prestige)