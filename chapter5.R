##-------------------------------------------------------##
## An R Companion to Applied Regression, Second Edition  ##
##    Script for Chapter 5                               ##
##                                                       ##
##    John Fox and Sanford Weisberg                      ##
##    Sage Publications, 2011                            ##
##-------------------------------------------------------##

 
options(show.signif.stars=FALSE)



set.seed(100) # to reproduce results in the text
library(car)
some(Mroz)  # sample 10 rows
nrow(Mroz)
mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
   family=binomial, data=Mroz)

mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
   family=binomial(link=logit), data=Mroz)
summary(mroz.mod)
round(exp(cbind(Estimate=coef(mroz.mod), confint(mroz.mod))), 2)
mroz.mod.2 <- update(mroz.mod, . ~ . - k5 - k618)
anova(mroz.mod.2, mroz.mod, test="Chisq")
Anova(mroz.mod)
head(predict(mroz.mod)) # first 6 values
head(predict(mroz.mod, type="response"))

closeness <- factor(rep(c("one.sided", "close"), c(3, 3)),
    levels=c("one.sided", "close"))
preference <- factor(rep(c("weak", "medium", "strong"), 2),
    levels=c("weak", "medium", "strong"))
voted <- c(91, 121, 64, 214, 284, 201)
did.not.vote <- c(39, 49, 24, 87, 76, 25)
logit.turnout <- log(voted/did.not.vote)
Campbell <- data.frame(closeness, preference, voted, did.not.vote,
     logit=logit.turnout)
Campbell
oldpar <- par(mar=c(5.1, 4.1, 4.1, 4.1)) # add room for right-side axis
with(Campbell,
    interaction.plot(preference, closeness, logit,
        type="b", pch=c(1, 16), cex=2, ylab="log(Voted/Did Not Vote)"))
probabilityAxis(side="right", at=seq(0.7, 0.875, by=0.025),
    axis.title="Proportion(Voted)")  # right y-axis
par(oldpar) # restore default margins


campbell.mod <- glm(cbind(voted, did.not.vote) ~
    closeness*preference, family=binomial, data=Campbell)
summary(campbell.mod)
predict(campbell.mod, type="link")
campbell.mod.2 <- update(campbell.mod,
    . ~ . - closeness:preference) # no interactions
anova(campbell.mod.2, campbell.mod, test="Chisq")
Anova(campbell.mod)
c(df=df.residual(campbell.mod.2), Test=deviance(campbell.mod.2))

set.seed(123456) # to reproduce results in the text
Campbell.long <- data.frame(close=NULL, prefer=NULL,
    turn=NULL) # initialize an empty data frame
for (j in 1:6) { # loop over combinations of factors
  x1 <- with(Campbell,
    data.frame(close=closeness[j],
        prefer=preference[j],
        turn=rep("did.not.vote", did.not.vote[j]))) # non-voters rows
  x2 <- with(Campbell,
    data.frame(close=closeness[j],
        prefer=preference[j],
        turn=rep("voted", voted[j]))) # rows for voters
 Campbell.long <- rbind(Campbell.long, x1, x2) # build up rows
}
some(Campbell.long) # sample rows
nrow(Campbell.long)
ftable(xtabs(~ close + prefer + turn, data=Campbell.long))
campbell.mod.long <- glm(turn ~ close*prefer,
    family=binomial, data=Campbell.long)
summary(campbell.mod.long)
Anova(campbell.mod.long)

set.seed(12345) # to reproduce the results in the text
some(Ornstein)
nrow(Ornstein)


(tab <- xtabs(~ interlocks, data=Ornstein))
x <- as.numeric(names(tab)) # names are distinct values of interlocks
plot(x, tab, type="h", xlab="Number of Interlocks", ylab="Frequency")
points(x, tab, pch=16)
mod.ornstein <- glm(interlocks ~ log2(assets) + nation + sector,
   family=poisson, data=Ornstein)
summary(mod.ornstein)
Anova(mod.ornstein)
exp(coef(mod.ornstein))

library(effects)
plot(allEffects(mod.ornstein, default.levels=50), ask=FALSE)

AMSsurvey$class <- NULL # delete this column from the data frame
head(AMSsurvey)  # first 6 rows
nrow(AMSsurvey)
(tab.sex.citizen <- xtabs(count ~ sex + citizen, data=AMSsurvey))
chisq.test(tab.sex.citizen, correct=FALSE) # suppress Yates correction
(AMS2 <- as.data.frame(tab.sex.citizen))
(phd.mod.indep <- glm(Freq ~ sex + citizen, family=poisson, data=AMS2))
pchisq(2.57, df=1, lower.tail=FALSE)
sum(residuals(phd.mod.indep, type="pearson")^2)
phd.mod.all <- glm(count ~ type*sex*citizen, # saturated model
    family=poisson, data=AMSsurvey)
Anova(phd.mod.all)
summary(phd.mod.1 <- update(phd.mod.all,
    . ~ .- sex:citizen - type:sex:citizen))
pchisq(1.9568, df=6, lower.tail=FALSE)

Campbell
library(reshape)
(Campbell1 <- melt(Campbell,
    id.vars=c("closeness", "preference"),
    measure.var=c("voted", "did.not.vote"),
    variable_name="turnout"))
mod.loglin <- glm(value ~ closeness*preference*turnout,
   family=poisson, data=Campbell1)
Anova(mod.loglin)

set.seed=12345 # to reproduce the results in the text
some(Salaries)
nrow(Salaries)
ftable(tab1 <- xtabs(~ rank + discipline + sex, data=Salaries))
(Salaries1 <- data.frame(tab1))

set.seed(101) # to reproduce the results in the text
some(Womenlf)
nrow(Womenlf)
library(nnet)
Womenlf$partic <- factor(Womenlf$partic,
    levels=c("not.work", "parttime", "fulltime"))
mod.multinom <- multinom(partic ~ hincome + children + region,
    data=Womenlf)
Anova(mod.multinom)
mod.multinom.1 <- update(mod.multinom, . ~ . - region)
summary(mod.multinom.1, Wald=TRUE)
library(effects)
plot(allEffects(mod.multinom.1), ask=FALSE)
plot(effect("hincome*children", mod.multinom.1))
Womenlf$working <- with(Womenlf,
       recode(partic, " 'not.work' = 'no'; else = 'yes' "))
Womenlf$fulltime <- with(Womenlf,recode (partic,
       " 'fulltime' = 'yes'; 'parttime' = 'no'; 'not.work' = NA "))
xtabs(~ partic + working, data=Womenlf)
xtabs(~ partic + fulltime, data=Womenlf)
mod.working <- glm(working ~ hincome + children + region,
  family=binomial, data=Womenlf)
summary(mod.working)
mod.fulltime <- update(mod.working, fulltime ~ .)
summary(mod.fulltime)
Anova(mod.working)
Anova(mod.fulltime)
mod.working.1 <- update(mod.working, . ~ . - region)
mod.fulltime.1 <- update(mod.fulltime, . ~ . - region)

(Predictors <- expand.grid(hincome=1:45,
               children=c("absent", "present")))
p.work <- predict(mod.working.1, newdata=Predictors, type="response")
p.fulltime <- predict(mod.fulltime.1, newdata=Predictors,
               type="response")
p.full <- p.work*p.fulltime
p.part <- p.work*(1 - p.fulltime)
p.not <- 1 - p.work
par(mfrow=c(1, 2))  # 1 row and 2 columns of panels
plot(c(1, 45), c(0, 1),
    type="n", xlab="Husband's Income", ylab="Fitted Probability",
    main="Children Absent")
lines(1:45, p.not[1:45], lty="solid", lwd=3)   # not working
lines(1:45, p.part[1:45], lty="dashed", lwd=3)  # part-time
lines(1:45, p.full[1:45], lty="dotted", lwd=3)  # full-time
legend("topright", lty=1:3, lwd=3, cex=0.75, inset=0.01,
    legend=c("not working", "part-time", "full-time"))
plot(c(1, 45), c(0, 1),
    type="n", xlab="Husband's Income", ylab="Fitted Probability",
    main="Children Present")
lines(1:45, p.not[46:90], lty="solid", lwd=3)
lines(1:45, p.part[46:90], lty="dashed", lwd=3)
lines(1:45, p.full[46:90], lty="dotted", lwd=3)
par(mfrow=c(1, 1))

library(MASS) # actually previously loaded by library(car)
mod.polr <- polr(partic ~ hincome + children, data=Womenlf)
summary(mod.polr)
pchisq(deviance(mod.polr) - deviance(mod.multinom.1),
    df = 6 - 4, lower.tail=FALSE)
plot(effect("hincome*children", mod.polr))
plot(effect("hincome*children", mod.polr), style="stacked")
plot(effect("hincome*children", mod.polr, latent=TRUE))

trans.gamma <- glm(time ~ t1 + t2, family=Gamma(link=identity),
                data=Transact)
summary(trans.gamma)
gamma.shape(trans.gamma)

(phihat <- sum(residuals(mod.ornstein, type="pearson")^2)/
         df.residual(mod.ornstein))
summary(mod.ornstein, dispersion=phihat)
Anova(mod.ornstein, test="F")

mod.ornstein.q <- update(mod.ornstein, family=quasipoisson)

mod.ornstein.nb <- update(mod.ornstein, family=negative.binomial(1.5))
thetas <- seq(0.5, 2.5, by=0.5)
aics <- rep(0, 5) # allocate vector
for (i in seq(along=thetas)) aics[i] <- AIC(update(mod.ornstein.nb,
         family=negative.binomial(thetas[i])))
rbind(thetas, aics)
summary(mod.ornstein.nb)
summary(glm.nb(interlocks ~ log2(assets) + nation + sector,
     data=Ornstein))

args(glm)