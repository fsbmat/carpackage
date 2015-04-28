##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Chapter 6                               ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##

options(show.signif.stars=FALSE)

library(car)
prestige.mod.2 <- lm(prestige ~ education + income + type,
    data=Prestige)
residualPlots(prestige.mod.2)

residualPlots(prestige.mod.2, ~ 1, fitted=TRUE)
residualPlots(prestige.mod.2, ~ education, fitted=FALSE)

marginalModelPlots(prestige.mod.2)
avPlots(prestige.mod.2, id.n=2, id.cex=0.6)

mod.duncan <- lm(prestige ~ income + education, data=Duncan)
set.seed(12345) # to reproduce results in the text
qqPlot(mod.duncan, id.n=3)
outlierTest(mod.duncan)
influenceIndexPlot(mod.duncan, id.n=3)
mod.duncan.2 <- update(mod.duncan,
    subset= rownames(Duncan) != "minister")
compareCoefs(mod.duncan, mod.duncan.2)
influencePlot(mod.duncan, id.n=3)
avPlots(mod.duncan, id.n=3)
mod.duncan.3 <- update(mod.duncan,
    subset = !(rownames(Duncan) %in% c("minister", "conductor")))
compareCoefs(mod.duncan, mod.duncan.2, mod.duncan.3, se=FALSE)
dfbs.duncan <- dfbetas(mod.duncan)
head(dfbs.duncan)  # first few rows
plot(dfbs.duncan[ , c("income", "education")])  # for b1 and b2
# to exit from identify, right-click on Windows, esc on Mac OS X
identify(dfbs.duncan[ , "income"], dfbs.duncan[ , "education"],
   rownames(Duncan))

mod.ornstein <- lm(interlocks + 1 ~ log(assets) + nation + sector,
    data=Ornstein)
par(mfrow=c(1, 2))
qqPlot(mod.ornstein, id.n=0)
plot(density(rstudent(mod.ornstein)))
boxCox(mod.ornstein, lambda = seq(0, 0.6, by=0.1))
summary(p1 <- powerTransform(mod.ornstein))
par(mfrow=c(1, 1))

Ornstein1 <- transform(Ornstein,
   y1=bcPower(interlocks + 1, coef(p1)),
   y1round=bcPower(interlocks + 1, coef(p1, round=TRUE)))
mod.ornstein.trans <- update(mod.ornstein, y1round ~ ., data=Ornstein1)
mod.ornstein.cv <- update(mod.ornstein,
    . ~ . + boxCoxVariable(interlocks + 1))
summary(
  mod.ornstein.cv)$coef["boxCoxVariable(interlocks + 1)", , drop=FALSE]
avPlots(mod.ornstein.cv, "boxCoxVariable(interlocks + 1)")

(wool.mod <- lm(cycles ~ len + amp + load, data=Wool))
inverseResponsePlot(wool.mod, id.n=4)
summary(powerTransform(wool.mod))

prestige.mod.3 <- update(prestige.mod.2, ~ . - type + women)
crPlots(prestige.mod.3, order=2)
prestige.mod.4 <- update(prestige.mod.3,~ . + log2(income) - income )
prestige.mod.5 <- update(prestige.mod.4,
    . ~ . - women + poly(women, 2))
summary(prestige.mod.5)$coef

boxTidwell(prestige ~ income + education,
    other.x = ~ poly(women, 2), data=Prestige)
mod.prestige.cv <- lm(prestige ~ income + education + poly(women, 2)
    + I(income * log(income)) + I(education * log(education)),
    data=Prestige)
summary(
    mod.prestige.cv)$coef["I(income * log(income))", , drop=FALSE]
summary(
    mod.prestige.cv)$coef["I(education * log(education))", , drop=FALSE]

par(mfrow=c(1, 2))
avPlot(mod.prestige.cv, "I(income * log(income))", id.n=0)
avPlot(mod.prestige.cv, "I(education * log(education))", id.n=0)
par(mfrow=c(1, 1))

residualPlots(mod.ornstein, ~ 1, fitted=TRUE, id.n=0,
    quadratic=FALSE, tests=FALSE)
spreadLevelPlot(mod.ornstein)
ncvTest(mod.ornstein)
ncvTest(mod.ornstein, ~ log(assets) + nation + sector, data=Ornstein)
mod.ornstein.wts <- update(mod.ornstein, weights = 1/log(assets))

mod.working <- glm(partic != "not.work" ~ hincome + children,
     family=binomial, data=Womenlf)
summary(mod.working)
residualPlots(mod.working, layout=c(1,3))
influenceIndexPlot(mod.working, vars=c("Cook", "hat"), id.n=3)
compareCoefs(mod.working, update(mod.working, subset=-c(76, 77)))

mod.ornstein.pois <- glm(interlocks ~ assets + nation + sector,
    family=poisson, data=Ornstein)
crPlots(mod.ornstein.pois, "assets")
mod.ornstein.pois.2 <- update(mod.ornstein.pois,
    . ~ log2(assets) + nation + sector)
crPlots(mod.ornstein.pois.2, "log2(assets)")
mod.ornstein.pois.cv <- update(mod.ornstein.pois,
    . ~ . + I(assets*log(assets)))
avPlots(mod.ornstein.pois.cv, "I(assets * log(assets))", id.n=0)
summary(
    mod.ornstein.pois.cv)$coef["I(assets * log(assets))", , drop=FALSE]
    
mod.mroz <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
    family=binomial, data=Mroz)
crPlots(mod.mroz, "lwg", pch=as.numeric(Mroz$lfp))
legend("bottomleft",c("Estimated lwg", "Observed lwg"),
    pch=1:2, inset=0.01)
    
head(Ericksen)
mod.census <- lm(undercount ~ ., data=Ericksen)
summary(mod.census)
vif(mod.census)

vif(mod.ornstein)
vif(mod.ornstein.pois.2)