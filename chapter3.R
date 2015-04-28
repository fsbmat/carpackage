##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Chapter 3                               ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##


options(show.signif.stars=FALSE)

library(car)
head(Prestige) # first 6 rows
with(Prestige, hist(income))
with(Prestige, hist(income, breaks="FD", col="gray"))
box()

args(hist.default)

with(Prestige, {
   hist(income, breaks="FD", freq=FALSE, ylab="Density")
   lines(density(income), lwd=2)
   lines(density(income, adjust=0.5), lwd=1)
   rug(income)
   box()
})
with(Prestige, qqPlot(income, labels=row.names(Prestige), id.n=3))
set.seed(124) # for reproducibility
qqPlot(rchisq(100, 3), distribution="chisq", df=3)
Boxplot(~ income, data=Prestige)
with(Prestige, plot(income, prestige))
scatterplot(prestige ~ income, span=0.6, lwd=3,
    id.n=4, data=Prestige)
scatterplot(prestige ~ income | type, data=Prestige, boxplots=FALSE,
             span=0.75, col=gray(c(0, .25, .5)), id.n=0)

head(Vocab)
nrow(Vocab)
plot(vocabulary ~ education, data=Vocab)
plot(jitter(vocabulary) ~ jitter(education), data= Vocab)
plot(jitter(vocabulary, factor=2) ~ jitter(education, factor=2),
    col="gray", cex=0.5, data=Vocab)
with(Vocab, {
    abline(lm(vocabulary ~ education), lwd=3, lty="dashed")
    lines(lowess(education, vocabulary, f=0.2), lwd=3)
    })


set.seed(1234) # to reproduce results in the text

some(Ornstein) # sample 10 rows
nrow(Ornstein)
Boxplot(interlocks ~ nation, data=Ornstein, main="(a)")
library(plotrix)
means <- with(Ornstein, tapply(interlocks, nation, mean))
sds <- with(Ornstein, tapply(interlocks, nation, sd))
plotCI(1:4, means, sds, xaxt="n", xlab="Nation of Control",
    ylab="interlocks", main="(b)", ylim=c(0, 100))
lines(1:4, means)
axis(1, at=1:4, labels = names(means))

scatter3d(prestige ~ income + education, id.n=3, data=Duncan)

scatterplotMatrix(~ prestige + income + education + women,
    span=0.7, id.n=0, data=Prestige)
log(7) # natural logarithm
log10(7) # base-10 logarithm
log2(7) # base-2 logarithm
log2(7)/log2(exp(1)) # again the natural logarithm
log(7, base=10)  # equivalent to log10(7)
logb(7, 10)
par(mfrow=c(1, 2))
with(Ornstein, plot(density(assets), xlab="assets", main="(a)"))
with(Ornstein, plot(density(log10(assets)),
     xlab="base-10 log of assets", main="(b)"))
par(mfrow=c(1,1))
scatterplot(infant.mortality ~ gdp, data=UN, xlab="GDP per Capita",
     ylab="Infant Mortality Rate (per 1000 births)", main="(a)",
     boxplot=FALSE)
scatterplot(infant.mortality ~ gdp, data=UN, xlab="GDP per capita",
     ylab="Infant Mortality Rate (per 1000 births)", main="(b)",
     log="xy", boxplots=FALSE, id.n=4)
lm(log(infant.mortality) ~ log(gdp), data=UN)
bcPower(1:5, 0.5)
yjPower(-5:5, 0.5)
symbox(~ gdp, data=UN)
asin(sqrt(seq(0, 1, length=11)))

logit(seq(0.1, 0.9, 0.1))
logit(seq(0, 1, 0.1))
par(mfrow=c(1, 3))
with(Prestige, {
    plot(density(women, from=0, to=100),
        main="(a) Untransformed")
    plot(density(logit(women), adjust=0.75),
        main="(b) Logit")
    plot(density(asin(sqrt(women/100)),
        adjust=0.75), main="(c) Arcsine square-root")
})
par(mfrow=c(1, 1))

spreadLevelPlot(interlocks + 1 ~ nation, Ornstein)
oldmar <- par(mar=c(5.1, 4.1, 4.1, 4.1))
Boxplot(log10(interlocks + 1) ~ nation, data=Ornstein)
basicPowerAxis(power=0, base=10, at=c(1, 3, 6, 11, 21, 51, 101),
     start=1, axis.title="Interlocks")
par(oldmar)
par(mfrow=c(1, 2))
invTranPlot(prestige ~ income, data=Prestige, lwd=2,
    xlab="income", main="(a)", col.lines=gray((0:3)/6))
plot(prestige ~ I(income^(1/3)), data=Prestige,
    xlab=expression(income^{1/3}), main="(b)")
abline(lm(prestige ~ I(income^(1/3)), data=Prestige))
summary(powerTransform(UN))
par(mfrow=c(1, 1))

with(UN, summary(powerTransform(cbind(infant.mortality, gdp))))
summary(powerTransform(UN[ , c("infant.mortality", "gdp")]))

summary(powerTransform(interlocks ~ nation,
    data=Ornstein, family="yjPower"))
summary(p1 <- with(Prestige,
    powerTransform(cbind(income, education))))
testTransform(p1, lambda=c(0.33, 1))
testTransform(p1, lambda=c(0, 1))
scatterplotMatrix(~ prestige + log2(income) + education + women,
     span=0.7, Prestige, id.n=0)
coef(p1)
coef(p1, round=TRUE)
Prestige <- transform(Prestige, log2income = log2(income))
summary(p2 <- powerTransform(cbind(income, education) ~ type,
    data=Prestige))
testTransform(p2, c(0, 1))
scatterplotMatrix(~ prestige + log2(income) + education + women | type,
      Prestige, by.group=TRUE, id.n=0, smooth=FALSE,
      col=gray(c(0, .25, .5)))

with(Freedman, {
    plot(density, crime)
    # to exit from identify: right-click in Windows, esc in Mac OS X
    identify(density, crime, row.names(Freedman))
})

args(showLabels)