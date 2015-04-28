##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Chapter 4                               ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##


options(show.signif.stars=FALSE)

library(car)
head(Davis) # first 6 rows
nrow(Davis)
davis.mod <- lm(weight ~ repwt, data=Davis)
davis.mod
summary(davis.mod)
confint(davis.mod)
scatterplot(weight ~ repwt, data=Davis, smooth=FALSE, id.n=1)
davis.mod.2 <- update(davis.mod, subset=-12)
summary(davis.mod.2)
cbind(Original=coef(davis.mod), NoCase12=coef(davis.mod.2))
head(Prestige)
nrow(Prestige)
prestige.mod <- lm(prestige ~ education + log2(income) + women,
    data=Prestige)
summary(prestige.mod)
Prestige$type
class(Prestige$type)
levels(Prestige$type)
Prestige$type <- with(Prestige, factor(type,
    levels=c("bc", "wc", "prof")))
select <- c(1, 2, 35, 36, 61, 62) # a few rows
Prestige$type[select]  # a few values
type.number <- as.numeric(Prestige$type)
type.number[select]
class(type.number)
type.character <- as.character(Prestige$type)
type.character[select]
class(type.character)
type.factor <- factor(type.character, levels=c("bc", "wc", "prof"))
type.factor[select]

(z <- factor(rep(c("a", "b", "c", "d"), c(3, 2, 4, 1))))
model.matrix(~ z)
contrasts(z)

set.seed(1234) # to reproduce results in the text

some(Baumann) # sample 10 observations
nrow(Baumann)
xtabs(~ group, data=Baumann)
with(Baumann, tapply(post.test.3, group, mean))
with(Baumann, tapply(post.test.3, group, sd))
plot(post.test.3 ~ group, data=Baumann, xlab="Group",
     ylab="Reading Score")
baum.mod.1 <- lm(post.test.3 ~ group, data=Baumann)
summary(baum.mod.1)
summary(update(baum.mod.1, . ~ . - group +
    relevel(group, ref="DRTA")))
prestige.mod.1 <- update(prestige.mod, ~ . - women + type)
summary(prestige.mod.1)
library(effects)
effect("type", prestige.mod.1)
select # defined previously
model.matrix(~ type + education + education:type,
  data=Prestige)[select, ]
prestige.mod.2 <- update(prestige.mod.1,
    . ~ . + log2(income):type + education:type)
summary(prestige.mod.2)

prestige.mod.2a <- lm(prestige ~ education*type + log2(income)*type,
                      data=Prestige)
prestige.mod.2b <- lm(prestige ~ type*(education + log2(income)),
                      data=Prestige)

set.seed(123456) # to reproduce the results in the text

some(Moore)  # sample 10 rows
nrow(Moore)
Moore$fcategory <- factor(Moore$fcategory,
    levels=c("low", "medium", "high"))
Moore$partner.status <- relevel(Moore$partner.status, ref="low")
xtabs(~ fcategory + partner.status, data=Moore)
with(Moore, tapply(conformity,
    list(Authoritarianism=fcategory,
        "Partner's Status"=partner.status),
    mean))
with(Moore, tapply(conformity,
    list(Authoritarianism=fcategory,
        "Partner's Status"=partner.status),
    sd))
boxplot(conformity ~ fcategory:partner.status, data=Moore,
     xlab="Condition", ylab="conformity")
abline(v=3.5, lty="dashed")
mod.moore.1 <- lm(conformity ~ fcategory*partner.status, data=Moore)
summary(mod.moore.1)
mod.moore.2 <- update(mod.moore.1, . ~ . - fcategory:partner.status)
summary(mod.moore.2)

set.seed(12345) # to reproduce the results in the text

with(Moore, {
    interaction.plot(fcategory, partner.status, conformity, type="b",
        pch=c(1, 16), cex=2, ylim=range(conformity), leg.bty="o")
    points(jitter(as.numeric(fcategory), factor=0.5), conformity,
        pch=ifelse(partner.status == "low", "L", "H"))
    })
    
confint(prestige.mod.1)
coef(prestige.mod.1)
cbind(Estimate=coef(prestige.mod.1), confint(prestige.mod.1))
plot(effect("group", baum.mod.1))
plot(allEffects(prestige.mod, default.levels=50), ask=FALSE)
plot(allEffects(mod.moore.2), ask=FALSE)
plot(effect("fcategory:partner.status", mod.moore.2))
plot(allEffects(prestige.mod.2, default.levels=50), ask=FALSE,
     layout=c(1, 3))

set.seed(12345) # to reproduce the results in the text

some(SLID)
nrow(SLID)
library(splines)
mod.slid.1 <- lm(log(wages) ~ sex + ns(education, df=6) +
    ns(age, df=6), data=SLID,
    subset = age >= 18 & age <= 65 & education >= 6)
Anova(mod.slid.1)
summary(mod.slid.1)
plot(allEffects(mod.slid.1,
        xlevels=list(age=18:65, education=6:20),
        given.values=c(sexMale=0.5),
        transformation=list(link=log, inverse=exp)),
    ylab="Composite Hourly Wages", ask=FALSE)
mod.slid.2 <- lm(log(wages) ~ sex + education + poly(age, 2), data=SLID,
    subset = age >= 18 & age <= 65 & education >= 6)
summary(mod.slid.2)
plot(allEffects(mod.slid.2,
        xlevels=list(age=18:65, education=6:20),
        given.values=c(sexMale=0.5),
        transformation=list(link=log, inverse=exp)),
    ylab="Composite Hourly Wages", ask=FALSE)
(V <- vcov(prestige.mod.1))
sqrt(diag(V))
round(cov2cor(V), 3)
(coefmat <- summary(prestige.mod.1)$coef)
Prestige.1 <- with(Prestige, data.frame(
    prestige=prestige, log2income=log2(income), education=education,
    women=women))
Prestige.scaled <- data.frame(scale(Prestige.1))
head(Prestige.scaled)
summary(lm(prestige ~ log2income + education + women,
    data=Prestige.scaled))

set.seed(12345) # to reproduce the results in the text

some(Transact)
nrow(Transact)
summary(trans.mod <- lm(time ~ t1 + t2, data=Transact))
vcov(trans.mod)
hccm(trans.mod)
library(lmtest)
coeftest(trans.mod, vcov=hccm)

set.seed(3435) # for reproducibility
betahat.boot <- bootCase(trans.mod, B=999)
usualEsts <- summary(trans.mod)$coef[ , 1:2]
bootSD <- apply(betahat.boot, 2, sd) # bootstrap standard errors
bootEst <- colMeans(betahat.boot)
bootBias <- (bootEst - usualEsts[ , 1])/usualEsts[ , 2]
bootCI <- apply(betahat.boot, 2, function(x) quantile(x, c(.025,.975)))
print(cbind(usualEsts, bootSD, bootEst, bootBias, t(bootCI)), digits=3)

duncan.mod <- lm(prestige ~ income + education, data=Duncan)
(ci <- confint(duncan.mod))

confidenceEllipse(duncan.mod, levels=c(0.85, 0.95))
for (j in 1:2) abline(v=ci[2, j], lty=2) # marginal CI for education
for (j in 1:2) abline(h=ci[3, j], lty=2) # marginal CI for income
with(Duncan, {
    dataEllipse(income, education, levels=c(0.5, 0.75, 0.9, 0.95))
    # to exit from identify, right-click in Windows, esc in Mac OS X
    identify(income, education, rownames(Duncan))
    })

tval <- (coef(davis.mod.2)[2] - 1)/sqrt(vcov(davis.mod.2)[2, 2])
pval <- 2*pt(abs(tval), df.residual(davis.mod.2), lower.tail=FALSE)
c(tval=tval, pval=pval)
prestige.mod.1 <- lm(prestige ~ education + log2(income) + type,
    data=na.omit(Prestige)) # full model
prestige.mod.0 <- update(prestige.mod.1, . ~ 1) # intercept only
anova(prestige.mod.0, prestige.mod.1) # compare models
prestige.mod.0inc <- update(prestige.mod.1, . ~ . - log2(income))
anova(prestige.mod.0inc, prestige.mod.1) # compare models
anova(prestige.mod.1)
Anova(prestige.mod.1)
prestige.mod.3 <- update(prestige.mod.1,
    . ~ . + log2(income):type + education:type)
Anova(prestige.mod.3)
Anova(moore.mod <- lm(conformity ~ fcategory*partner.status, data=Moore))
contrasts(Moore$fcategory) <-
      contrasts(Moore$partner.status) <- "contr.sum"
moore.mod.1 <- update(moore.mod)
Anova(moore.mod.1, type="III")
contrasts(Moore$fcategory) <- contrasts(Moore$partner.status) <- NULL
summary(trans.mod)$coef
linearHypothesis(trans.mod, c(0, 1, -1))
linearHypothesis(trans.mod, c(0, 1, -1), vcov=hccm)
compareCoefs(davis.mod, davis.mod.2)
diag(2)  # order-2 identity matrix
linearHypothesis(davis.mod, diag(2), c(0, 1))
linearHypothesis(davis.mod.2, diag(2), c(0, 1))

linearHypothesis(trans.mod, "t1 = t2")
linearHypothesis(davis.mod, c("(Intercept) = 0", "repwt = 1"))

deltaMethod(davis.mod.2, "(Intercept)/(1 - repwt)")
(d1 <- deltaMethod(trans.mod, "t1/t2"))
c("2.5%" = d1$Estimate - 1.96*d1$SE,
    "97.5%" = d1$Estimate + 1.96*d1$SE)

colnames(betahat.boot)
ratio <- betahat.boot[ , 2]/betahat.boot[ , 3] # t1/t3
c(est=coef(trans.mod)[2]/coef(trans.mod)[3],
  bootSE=sd(ratio), quantile(ratio, c(.025, .975)))

deltaMethod(trans.mod, "t1/t2", vcov=hccm)

set.seed(12345) # to reproduce results in the text
some(Salaries)
nrow(Salaries)
Salaries$rank <- relevel(Salaries$rank, ref="AsstProf")
ftable(x1 <- xtabs(~ discipline + rank + sex, data=Salaries))
round(100*ftable(prop.table(x1, margin=c(1, 2))), 1) # % m and f

library(lattice)
xyplot(salary ~ yrs.since.phd | discipline:rank, group=sex,
  data=Salaries, type=c("g", "p", "r"), auto.key=TRUE)

bwplot(salary ~ discipline:sex | rank, data=Salaries,
    scales=list(rot=90), layout=c(3, 1))

fselector <- Salaries$sex == "Female" # TRUE for females
salmod <- lm(salary ~ rank*discipline + yrs.since.phd, data=Salaries,
    subset=!fselector) # regression for males
# predictions for females:
femalePreds <- predict(salmod, newdata=Salaries[fselector, ])
(meanDiff <- mean(Salaries$salary[fselector] - femalePreds))

set.seed(8141976) # for reproducibility
fnumber <- sum(fselector) # number of females
n <- length(fselector) # number of observations
B <- 999 # number of replications
simDiff <- numeric(B) # initialize vector with B entries
for (j in 1:B){
    sel <- sample(1:n, fnumber) # random sample of nominated 'females'
    m2 <- update(salmod, subset=-sel) # refit regression model
    simDiff[j] <- mean(Salaries$salary[sel]
        - predict(m2, newdata=Salaries[sel, ])) # compute mean diff.
    }
(frac <- round(sum(meanDiff > simDiff)/(1 + B), 3))
hist(simDiff,
   main=paste("Histogram of Simulated Mean Differences\np-value =",
       frac),
   xlab="Dollars")
abline(v=meanDiff, lty="dashed")

summary(highway.mod <- lm(log2(rate) ~ log2(len) + log2(ADT) +
        log2(trks) + log2(sigs1) + slim + shld + lane + acpt +
        itg + lwid + hwy, data=Highway1))

highway.backward <- step(highway.mod, scope=list(lower= ~ log2(len)))
highway.backward
plot(fitted(highway.mod) ~ fitted(highway.backward))
abline(0, 1)
cor(fitted(highway.mod), fitted(highway.backward))
highway.mod0 <- update(highway.mod, . ~ log2(len))
(highway.forward <- step(highway.mod0, direction="forward",
    scope=list(lower = ~ log2(len),
        upper = ~ log2(len) + log2(ADT) + log2(trks) +
            log2(sigs1) + slim + shld + lane + acpt + itg + lwid + hwy),
   trace=0))
R <- cor(cbind(fitted(highway.mod), fitted(highway.backward),
    fitted(highway.forward)))
rownames(R) <- colnames(R) <- c("original", "backward", "forward")
R
AIC(highway.mod)
AIC(highway.backward)
AIC(highway.forward)
getOption("contrasts")
with(Prestige, contrasts(type))
Prestige$type1 <- relevel(Prestige$type, ref="prof")
with(Prestige, contrasts(type1))
contrasts(Prestige$type) <- "contr.SAS"
contrasts(Prestige$type)
contrasts(Prestige$type) <- "contr.helmert"
contrasts(Prestige$type)
contrasts(Prestige$type) <- "contr.sum"
contrasts(Prestige$type)
contrasts(Prestige$type) <- "contr.Treatment"
contrasts(Prestige$type)
contrasts(Prestige$type) <- "contr.Sum"
contrasts(Prestige$type)
options(contrasts = c("contr.sum", "contr.poly"))
Moore$fcategory1 <- ordered(Moore$fcategory)
Moore$fcategory1
round(contrasts(Moore$fcategory1), 2)
summary(lm(conformity ~ partner.status*fcategory1, data=Moore))
options(contrasts=c("contr.treatment",
        "contr.poly")) # return to defaults
dosage <- c(1, 4, 8, 1, 4, 8, 1, 4)
X <- poly(dosage, degree=2)
round(X, 3)
round(cor(X), 3)
poly(dosage, degree=2, raw=TRUE)
 
C <- matrix(c(1,-0.5,-0.5,  0,1,-1), 3, 2)
colnames(C) <- c("Basal vs. DRTA & Strat", "DRTA vs. Strat")
C
contrasts(Baumann$group) <- C
summary(lm(post.test.3 ~ group, data=Baumann))
summary(prestige.mod.4 <- update(prestige.mod.1, . ~ . - 1))
Anova(prestige.mod.4)
deleted.cell <-
     with(Moore, fcategory == "high" & partner.status=="high")
mod.moore.3 <- update(mod.moore.1, subset = !deleted.cell)
summary(mod.moore.3)
Anova(mod.moore.3)
args(lm)

lm(100*conformity/40 ~ partner.status*fcategory, data=Moore)

lm(logit(conformity/40) ~ partner.status*fcategory, data=Moore)
 
lm(prestige ~ I(income + education), data=Duncan)

(a <- factor(rep(LETTERS[1:3], each=3)))
(x <- rep(1:3, 3))
model.matrix(~ a/x)
model.matrix(~ x:a)

lm(weight ~ repwt, data=Davis,
     subset = sex == "F")  # fit only to women
lm(weight ~ repwt, data=Davis,
     subset=c(1:99, 141))  # use only obs. 1 to 99 and 141
lm(prestige ~ income + education, data=Duncan,
     subset=-c(6, 16)) # exclude obs. 6 and 16
lm(conformity ~ partner.status*fcategory, data=Moore,
    contrasts=list(partner.status=contr.sum, fcategory=contr.poly))

library(hints)
hints(prestige.mod)