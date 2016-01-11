## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch08")
.locals$ch08 <- NULL
.pkgs$ch08 <- NULL

## ----propodds, child="ch08/propodds.Rnw"---------------------------------

## ----arth-po0------------------------------------------------------------
data("Arthritis", package = "vcd")
head(Arthritis$Improved, 8)

## ----arth-po1------------------------------------------------------------
library(MASS)
arth.polr <- polr(Improved ~ Sex + Treatment + Age,
                  data = Arthritis, Hess = TRUE)
summary(arth.polr)

## ----arth-po2------------------------------------------------------------
library(car)
Anova(arth.polr)

## ----arth-vpo,size="footnotesize"----------------------------------------
library(VGAM)
arth.po <- vglm(Improved ~ Sex + Treatment + Age, data = Arthritis,
                family = cumulative(parallel = TRUE))
arth.po

## ----arth-vnpo,size="footnotesize"---------------------------------------
arth.npo <- vglm(Improved ~ Sex + Treatment + Age, data = Arthritis,
                 family = cumulative(parallel = FALSE))
arth.npo

## ----arth-coef-----------------------------------------------------------
coef(arth.po, matrix = TRUE)
coef(arth.npo, matrix = TRUE)

## ----arth-lrtest---------------------------------------------------------
VGAM::lrtest(arth.npo, arth.po)

## ----arth-vpo-npo--------------------------------------------------------
tab <- cbind(
  Deviance = c(deviance(arth.npo), deviance(arth.po)),
	df = c(df.residual(arth.npo), df.residual(arth.po))
	)
tab <- rbind(tab, diff(tab))
rownames(tab) <- c("GenLogit", "PropOdds", "LR test")
tab <- cbind(tab, pvalue=1-pchisq(tab[,1], tab[,2]))
tab

## ----arth.ppo------------------------------------------------------------
arth.ppo <- vglm(Improved ~ Sex + Treatment + Age, data = Arthritis,
  family = cumulative(parallel = FALSE ~ Sex))
coef(arth.ppo, matrix = TRUE)

## ----arth-rms1-----------------------------------------------------------
library(rms)
arth.po2 <- lrm(Improved ~ Sex + Treatment + Age, data = Arthritis)
arth.po2

## ----arth-rmsplot, h=4, w=12, out.width='\\textwidth', cap='Visual assessment of ordinality and the proportional odds assumption for predictors in the Arthritis data. Solid lines connect the stratified means of X given Y. Dashed lines show the estimated expected value of X given Y=j if the proportional odds model holds for X.'----
op <- par(mfrow=c(1,3))
plot.xmean.ordinaly(Improved ~ Sex + Treatment + Age, data=Arthritis,
                    lwd=2, pch=16, subn=FALSE)
par(op)

## ----arth-po3------------------------------------------------------------
arth.fitp <- cbind(Arthritis,
                  predict(arth.polr, type = "probs"))
head(arth.fitp)

## ----arth-po4------------------------------------------------------------
library(reshape2)
plotdat <- melt(arth.fitp,
                id.vars = c("Sex", "Treatment", "Age", "Improved"),
                measure.vars = c("None", "Some", "Marked"),
                variable.name = "Level",
                value.name = "Probability")
## view first few rows
head(plotdat)

## ----arth-polr1, h=8, w=8, out.width='.8\\textwidth', cap='Predicted probabilities for the proportional odds model fit to the Arthritis data.', fig.pos='!htb'----
library(ggplot2)
library(directlabels)
gg <- ggplot(plotdat, aes(x = Age, y = Probability, colour = Level)) +
    geom_line(size = 2.5) + theme_bw() + xlim(10, 80) +
    geom_point(color = "black", size = 1.5) +
    facet_grid(Sex ~ Treatment,
               labeller = function(x, y) sprintf("%s = %s", x, y)
               )
direct.label(gg)

## ----arth-po-eff1, h=5, w=4, out.width='.49\\textwidth', cap='Effect plots for the effect of Age in the proportional odds model for the Arthritis data.  Left: responses shown in separate panels. Right: responses shown in stacked format.'----
library(effects)
plot(Effect("Age", arth.polr))
plot(Effect("Age", arth.polr), style = "stacked",
     key.args = list(x = .55, y = .9))

## ----arth-po-eff2, h=6, w=8, out.width='.9\\textwidth', cap='Effect plot for the effects of Treatment, Sex, and Age in the Arthritis data.'----
plot(Effect(c("Treatment", "Sex", "Age"), arth.polr),
     style = "stacked", key.arg = list(x = .8, y = .9))

## ----arth-po-eff3, h=4, w=8, out.width='.9\\textwidth', cap='Latent variable effect plot for the effects of Treatment and Age in the Arthritis data.'----
plot(Effect(c("Treatment", "Age"), arth.polr, latent = TRUE), lwd = 3)


## ----nested, child="ch08/nested.Rnw"-------------------------------------

## ----wlf1----------------------------------------------------------------
library(car)   # for data and Anova()
data("Womenlf", package = "car")
some(Womenlf)

## ----wlf2----------------------------------------------------------------
# create dichotomies
Womenlf <- within(Womenlf,{
  working <-  recode(partic, " 'not.work' = 'no'; else = 'yes' ")
  fulltime <- recode(partic,
    " 'fulltime' = 'yes'; 'parttime' = 'no'; 'not.work' = NA")})
some(Womenlf)

## ----wlf3----------------------------------------------------------------
with(Womenlf, table(partic, working))
with(Womenlf, table(partic, fulltime, useNA = "ifany"))

## ----wlf-mod.working-----------------------------------------------------
mod.working <- glm(working ~ hincome + children, family = binomial,
                   data = Womenlf)
summary(mod.working)

## ----wlf-mod.fulltime----------------------------------------------------
mod.fulltime <- glm(fulltime ~ hincome + children, family = binomial,
                    data = Womenlf)
summary(mod.fulltime)

## ----wlf-coef------------------------------------------------------------
cbind(working = coef(mod.working), fulltime = coef(mod.fulltime))

## ----wlf-lrtest----------------------------------------------------------
LRtest <- function(model)
  c(LRchisq = model$null.deviance - model$deviance,
    df = model$df.null - model$df.residual)
    
tab <- rbind(working = LRtest(mod.working),
             fulltime = LRtest(mod.fulltime))
tab <- rbind(tab, All = colSums(tab))
tab <- cbind(tab, pvalue = 1- pchisq(tab[,1], tab[,2]))
tab

## ----wlf-anova, R.options=list(digits=8)---------------------------------
Anova(mod.working)
Anova(mod.fulltime)

## ----wlf-fitted1, size='footnotesize'------------------------------------
predictors <- expand.grid(hincome = 1 : 50,
                          children =c('absent', 'present'))
fit <- data.frame(predictors,
    p.working = predict(mod.working, predictors, type = "response"),
    p.fulltime = predict(mod.fulltime, predictors, type = "response"),
    l.working = predict(mod.working, predictors, type = "link"),
    l.fulltime = predict(mod.fulltime, predictors, type = "link")
)
print(some(fit, 5), digits = 3)

## ----wlf-fitted2---------------------------------------------------------
fit <- within(fit, {
  `full-time` <- p.working * p.fulltime
  `part-time` <- p.working * (1 - p.fulltime)
  `not working` <- 1 - p.working
  })

## ----wlf-reshape---------------------------------------------------------
fit2 <- melt(fit,
             measure.vars = c("full-time", "part-time", "not working"),
             variable.name = "Participation",
             value.name = "Probability")

## ----wlf-fitted-prob, h=4, w=8, out.width='.8\\textwidth', cap="Fitted probabilities from the models for nested dichotomies fit to the data on women's labor force participation."----
gg <- ggplot(fit2,
             aes(x = hincome, y = Probability, colour= Participation)) + 
        facet_grid(~ children, 
	           labeller = function(x, y) sprintf("%s = %s", x, y)) + 
        geom_line(size = 2) + theme_bw() +
        scale_x_continuous(limits = c(-3, 55)) +
        scale_y_continuous(limits = c(0, 1))

direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

## ----wlf-fitted-logit, h=4, w=8, out.width='.8\\textwidth', cap="Fitted log odds from the models for nested dichotomies fit to the data on women's labor force participation."----
fit3 <- melt(fit,
             measure.vars = c("l.working", "l.fulltime"),
             variable.name = "Participation",
             value.name = "LogOdds")
levels(fit3$Participation) <- c("working", "full-time")

gg <- ggplot(fit3,
             aes(x = hincome, y = LogOdds, colour = Participation)) + 
        facet_grid(~ children, 
	           labeller = function(x, y) sprintf("%s = %s", x, y)) + 
        geom_line(size = 2) + theme_bw() +
        scale_x_continuous(limits = c(-3, 50)) +
        scale_y_continuous(limits = c(-5, 4))
        
direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))


## ----genlogit, child="ch08/genlogit.Rnw"---------------------------------

## ----wlf-glogit1---------------------------------------------------------
levels(Womenlf$partic)

## ----wlf-glogit2---------------------------------------------------------
# choose not working as baseline category
Womenlf$partic <- relevel(Womenlf$partic, ref = "not.work")

## ----wlf-glogit3---------------------------------------------------------
library(nnet)
wlf.multinom <- multinom(partic ~ hincome + children,
                         data = Womenlf, Hess = TRUE)

## ----wlf-glogit4---------------------------------------------------------
summary(wlf.multinom, Wald = TRUE)

## ----wlf-glogit5---------------------------------------------------------
stats <- summary(wlf.multinom, Wald = TRUE)
z <- stats$Wald.ratios
p <- 2 * (1 - pnorm(abs(z)))
zapsmall(p)

## ----wlf-glogit6---------------------------------------------------------
wlf.multinom2 <- multinom(partic ~ hincome * children,
                         data = Womenlf, Hess = TRUE)
Anova(wlf.multinom2)

## ----wlf-glogit7---------------------------------------------------------
predictors <- expand.grid(hincome = 1 : 50,
                          children = c("absent", "present"))
fit <- data.frame(predictors,
                  predict(wlf.multinom, predictors, type = "probs")
                  )

## ----wlf-multi-prob, h=4.5, w=8, out.width='.9\\textwidth', cap="Fitted probabilities from the generalized logit model fit to the data on women's labor force participation."----
fit2 <- melt(fit,
             measure.vars = c("not.work", "fulltime", "parttime"),
             variable.name = "Participation",
             value.name = "Probability")
levels(fit2$Participation) <- c("not working", "full-time", "part-time")

gg <- ggplot(fit2,
             aes(x = hincome, y = Probability, colour = Participation)) + 
        facet_grid(~ children, 
	           labeller = function(x, y) sprintf("%s = %s", x, y)) + 
        geom_line(size = 2) + theme_bw() +
        scale_x_continuous(limits = c(-3, 50)) +
        scale_y_continuous(limits = c(0, 0.9))   

direct.label(gg, list("top.bumptwice", dl.trans(y = y + 0.2)))

## ----wlf-ordered---------------------------------------------------------
levels(Womenlf$partic)
Womenlf$partic <- ordered(Womenlf$partic,
                          levels=c("not.work", "parttime", "fulltime"))
wlf.multinom <- update(wlf.multinom, . ~ .)

## ----wlf-multi-effect, h=5, w=8, out.width='.8\\textwidth', cap="Effect plot for the probabilities of not working and working part time and full time from the generalized logit model fit to the women's labor force data."----
plot(Effect(c("hincome", "children"), wlf.multinom),
     style = "stacked", key.args = list(x = .05, y = .9))


