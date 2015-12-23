## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch07")
.locals$ch07 <- NULL
.pkgs$ch07 <- NULL
library(ggplot2)
theme_set(theme_bw())  # set default ggplot theme

## ----odds----------------------------------------------------------------
library(MASS)
p <- c(.05, .10, .25, .50, .75, .90, .95)
odds <- p / (1 - p)
data.frame(p,
           odds = as.character(fractions(odds)),
           logit = log(odds))

## ----arth-age0, echo=FALSE-----------------------------------------------
data("Arthritis", package = "vcd")
Arthritis$Better <- as.numeric(Arthritis$Improved > "None")

## ----arth-logi-hist, h=5, w=6, out.width='.6\\textwidth', cap='Plot of the Arthritis treatment data, showing the conditional distributions of the 0/1 observations of the Better response by histograms and boxplots.', echo=-1----
source("functions/logi.hist.plot.R")
with(Arthritis,
     logi.hist.plot(Age, Improved > "None", type = "hist", 
                    counts = TRUE, ylabel = "Probability (Better)", 
                    xlab = "Age", col.cur = "blue", 
                    col.hist = "lightblue", col.box = "lightblue")
  )

## ----arth-age1-----------------------------------------------------------
data("Arthritis", package = "vcd")
Arthritis$Better <- as.numeric(Arthritis$Improved > "None")

## ----arth-age2-----------------------------------------------------------
arth.logistic <- glm(Better ~ Age, data = Arthritis, family = binomial)

## ----arth-age2a----------------------------------------------------------
library(lmtest)
coeftest(arth.logistic)

## ----arth-age3-----------------------------------------------------------
exp(coef(arth.logistic))
exp(10 * coef(arth.logistic)["Age"])

## ----arth-age4-----------------------------------------------------------
arth.lm <- glm(Better ~ Age, data = Arthritis)
coef(arth.lm)

## ----arth-test1----------------------------------------------------------
anova(arth.logistic, test = "Chisq")

## ----arth-test2----------------------------------------------------------
library(vcdExtra)
LRstats(arth.logistic)

## ----arthritis-age2, echo=FALSE, h=6, w=6, out.width='.6\\textwidth', cap='A version of plot of the Arthritis treatment data (\\figref{fig:arthritis-age}) produced with \\R base graphics, showing logistic, linear regression and lowess fits.', scap='A version of plot of the Arthritis treatment data produced with R base graphics', fig.pos="!b"----
plot(jitter(Better, .1) ~ Age, data = Arthritis,
     xlim = c(15, 85), pch = 16,
     ylab = "Probability (Better)")

xvalues <- seq(15, 85, 5)
pred.logistic <- predict(arth.logistic,
                         newdata=data.frame(Age=xvalues),
                         type="response", se.fit=TRUE)

upper <- pred.logistic$fit + 1.96*pred.logistic$se.fit
lower <- pred.logistic$fit - 1.96*pred.logistic$se.fit

polygon(c(xvalues, rev(xvalues)),
        c(upper, rev(lower)),
        col=rgb(0,0,1,.2), border=NA)
lines(xvalues, pred.logistic$fit, lwd = 4 , col = "blue")

abline(arth.lm, lwd=2)
lines(lowess(Arthritis$Age, Arthritis$Better, f = .9), 
      col = "red", lwd = 2)

## ----arth-plot1, eval=FALSE----------------------------------------------
## plot(jitter(Better, .1) ~ Age, data = Arthritis,
##      xlim = c(15, 85), pch = 16,
##      ylab="Probability (Better)")

## ----arth-plot2, eval=FALSE----------------------------------------------
## xvalues <- seq(15, 85, 5)
## pred.logistic <- predict(arth.logistic,
##                          newdata = data.frame(Age = xvalues),
##                          type = "response", se.fit = TRUE)

## ----arth-plot3, eval=FALSE----------------------------------------------
## upper <- pred.logistic$fit + 1.96 * pred.logistic$se.fit
## lower <- pred.logistic$fit - 1.96 * pred.logistic$se.fit

## ----arth-plot4, eval=FALSE----------------------------------------------
## polygon(c(xvalues, rev(xvalues)),
##         c(upper, rev(lower)),
##         col = rgb(0, 0, 1, .2), border = NA)
## lines(xvalues, pred.logistic$fit, lwd=4 , col="blue")

## ----arth-plot5, eval=FALSE----------------------------------------------
## abline(arth.lm, lwd = 2)
## lines(lowess(Arthritis$Age, Arthritis$Better, f = .9),
##       col = "red", lwd = 2)

## ----arth-gg1, eval=FALSE------------------------------------------------
## library(ggplot2)
## # basic logistic regression plot
## gg <- ggplot(Arthritis, aes(x = Age, y = Better)) +
##   xlim(5, 95) +
##   geom_point(position = position_jitter(height = 0.02, width = 0)) +
##   stat_smooth(method = "glm", family = binomial,
##               alpha = 0.1, fill = "blue", size = 2.5, fullrange = TRUE)

## ----arth-gg2, eval=FALSE------------------------------------------------
## # add linear model and loess smoothers
## gg <- gg + stat_smooth(method = "lm", se = FALSE,
##                        size = 1.2, color = "black", fullrange = TRUE)
## gg <- gg + stat_smooth(method = "loess", se = FALSE,
##                        span = 0.95, colour = "red", size = 1.2)
## gg  # show the plot

## ----nasa-temp1----------------------------------------------------------
data("SpaceShuttle", package = "vcd")
shuttle.mod <- glm(cbind(nFailures, 6 - nFailures) ~ Temperature,
          data = SpaceShuttle, na.action = na.exclude,
          family = binomial)

## ----nasa-temp2----------------------------------------------------------
SpaceShuttle$trials <- 6
shuttle.modw <- glm(nFailures / trials ~ Temperature, weight = trials,
          data = SpaceShuttle, na.action = na.exclude,
          family = binomial)

## ------------------------------------------------------------------------
all.equal(coef(shuttle.mod), coef(shuttle.modw))

## ------------------------------------------------------------------------
# testing, vs. null model
anova(shuttle.mod, test = "Chisq")

## ----nasa-temp-ggplot, h=6, w=8, out.width='.7\\textwidth', cap='Space shuttle data, with fitted logistic regression model.', eval=FALSE----
## library(ggplot2)
## ggplot(SpaceShuttle, aes(x = Temperature, y = nFailures / trials)) +
##   xlim(30, 81) +
##   xlab("Temperature (F)") +
##   ylab("O-Ring Failure Probability") +
##   geom_point(position=position_jitter(width = 0, height = 0.01),
##              aes(size = 2)) +
##   theme(legend.position = "none") +
##   geom_smooth(method = "glm", family = binomial, fill = "blue",
##               aes(weight = trials), fullrange = TRUE, alpha = 0.2,
##               size = 2)

## ----arth2-glm-----------------------------------------------------------
arth.logistic2 <- glm(Better ~ I(Age-50) + Sex + Treatment,
                      data = Arthritis,
                      family = binomial)

## ----arth2-glm2----------------------------------------------------------
coeftest(arth.logistic2)

## ----arth2-glm3, R.options=list(digits=4)--------------------------------
exp(cbind(OddsRatio = coef(arth.logistic2),
          confint(arth.logistic2)))

## ----arth-cond1, h=4, w=6, out.width='.6\\textwidth', cap='Conditional plot of Arthritis data showing separate points and fitted curves stratified by Treatment. A separate fitted curve is shown for the two treatment conditions, ignoring Sex.'----
library(ggplot2)
gg <- ggplot(Arthritis, aes(Age, Better, color = Treatment)) +
  xlim(5, 95) + theme_bw() +
  geom_point(position = position_jitter(height = 0.02, width = 0)) +
  stat_smooth(method = "glm", family = binomial, alpha = 0.2,
              aes(fill = Treatment), size = 2.5, fullrange = TRUE)
gg   # show the plot

## ----arth-cond2, h=4, w=8, out.width='.8\\textwidth', cap='Conditional plot of Arthritis data, stratified by Treatment and Sex. The unusual patterns in the panel for Males signals a problem with this data.', out.extra='clip'----
gg + facet_wrap(~ Sex)

## ----arth-margins--------------------------------------------------------
addmargins(xtabs(~Sex + Treatment, data = Arthritis), 2)

## ----arth-binreg0, fig.keep='none'---------------------------------------
library(vcd)
binreg_plot(arth.logistic2, type = "link")

## ----arth-binreg1, h=6, w=6, out.width='.49\\textwidth', cap='Full-model plot of Arthritis data, showing fitted logits by Treatment and Sex.', out.extra='clip'----
binreg_plot(arth.logistic2, type = "link", subset = Sex == "Female",
            main = "Female", xlim=c(25, 75), ylim = c(-3, 3))
binreg_plot(arth.logistic2, type = "link", subset = Sex == "Male",
            main = "Male", xlim=c(25, 75), ylim = c(-3, 3))

## ----arth-binreg2, h=6, w=6, out.width='.49\\textwidth', cap='Full-model plot of Arthritis data, showing fitted probabilities by Treatment and Sex.', out.extra='clip'----
binreg_plot(arth.logistic2, subset = Sex == "Female",
            main = "Female", xlim = c(25, 75))
binreg_plot(arth.logistic2, subset = Sex == "Male", 
            main = "Male", xlim = c(25, 75))


## ----arth-eff1-----------------------------------------------------------
library(effects)
arth.eff2 <- allEffects(arth.logistic2, partial.residuals = TRUE)
names(arth.eff2)

## ------------------------------------------------------------------------
arth.eff2[["Sex"]]
arth.eff2[["Sex"]]$model.matrix

## ----arth-effplot1, h=4, w=9, out.width='\\textwidth', cap='Plot of all effects in the main effects model for the Arthritis data. Partial residuals and their loess smooth are also shown for the continuous predictor, Age.', echo=FALSE, fig.pos="H"----
plot(arth.eff2, rows = 1, cols = 3,
     type="response", residuals.pch = 15)

## ----arth-effplot1-code, h=4, w=9, out.width='\\textwidth', cap='Plot of all effects in the main effects model for the Arthritis data. Partial residuals and their loess smooth are also shown for the continuous predictor, Age.', eval=FALSE, fig.keep='none'----
## plot(arth.eff2, rows = 1, cols = 3,
##      type="response", residuals.pch = 15)

## ----arth-full-----------------------------------------------------------
arth.full <- Effect(c("Age", "Treatment", "Sex"), arth.logistic2)

## ----arth-effplot2, h=4, w=8, out.width='.8\\textwidth', cap='Full-model plot of the effects of all predictors in the main effects model for the Arthritis data, plotted on the logit scale.'----
plot(arth.full, multiline = TRUE, ci.style = "bands",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = .52, y = .92, columns = 1), 
     grid = TRUE)

## ----arth-effplot3, h=4, w=8, out.width='.8\\textwidth', cap='Full-model plot of the effects of all predictors in the main effects model for the Arthritis data, plotted on the probability scale.'----
plot(arth.full, multiline = TRUE, ci.style = "bands", 
     type="response",
     colors = c("red", "blue"), lwd = 3,
     key.args = list(x = .52, y = .92, columns = 1), 
     grid = TRUE)

## ----donner1-ex, child="ch07/donner1.Rnw"--------------------------------

## ----donner11, echo=-1---------------------------------------------------
set.seed(1235)
data("Donner", package = "vcdExtra")   # load the data
library(car)                         # for some() and Anova()
some(Donner, 8)

## ----donner12------------------------------------------------------------
Donner$survived <- factor(Donner$survived, labels = c("no", "yes"))

## ----donner13------------------------------------------------------------
xtabs(~ family, data = Donner)

## ----donner14------------------------------------------------------------
# collapse small families into "Other"
fam <- Donner$family
levels(fam)[c(3, 4, 6, 7, 9)] <- "Other"

# reorder, putting Other last
fam = factor(fam,levels(fam)[c(1, 2, 4:6, 3)])
Donner$family <- fam
xtabs(~family, data=Donner)

## ----donner15------------------------------------------------------------
xtabs(~ survived + family, data = Donner)

## ----donner1-spineplot, h=4, w=8, out.width='.8\\textwidth', cap='Spineplot of survival in the Donner Party by family.'----
plot(survived ~ family, data = Donner, col = c("pink", "lightblue"))

## ----donner1-gpairs, h=6, w=6, out.width='.7\\textwidth', cap='Generalized pairs plot for the Donner data.'----
library(gpairs)
library(vcd)
gpairs(Donner[,c(4, 2, 3, 1)],
    diag.pars = list(fontsize = 20, hist.color = "gray"),
    mosaic.pars = list(gp = shading_Friendly), 
    outer.rot = c(45, 45)
)

## ----donner1-cond1, h=4, w=6, out.width='.7\\textwidth', cap='Conditional plot of the Donner data, showing the relationship of survival to age and sex. The smoothed curves and confidence bands show the result of fitting separate linear logistic regressions on age for males and females.'----
# basic plot: survived vs. age, colored by sex, with jittered points
gg <- ggplot(Donner, aes(age, as.numeric(survived=="yes"), 
                         color = sex)) + 
  ylab("Survived") + theme_bw() + 
  geom_point(position = position_jitter(height = 0.02, width = 0)) 

# add conditional linear logistic regressions
gg + stat_smooth(method = "glm", family = binomial, formula = y ~ x,
                 alpha = 0.2, size = 2, aes(fill = sex))

## ----donner1-cond3, h=5, w=6, out.width='.5\\textwidth', cap='Conditional plots of the Donner data, showing the relationship of survival to age and sex. Left: The smoothed curves and confidence bands show the result of fitting separate quadratic logistic regressions on age for males and females. Right: Separate loess smooths are fit to the data for males and females.'----
# add conditional quadratic logistic regressions
gg + stat_smooth(method = "glm", family = binomial, 
                 formula = y ~ poly(x,2), alpha = 0.2, size = 2, 
                 aes(fill = sex))

# add loess smooth
gg + stat_smooth(method = "loess", span=0.9, alpha = 0.2, size = 2, 
                 aes(fill = sex)) + 
  coord_cartesian(ylim = c(-.05,1.05))

## ----donner1-mod1--------------------------------------------------------
donner.mod1 <- glm(survived ~ age + sex,
                   data = Donner, family  =binomial)
Anova(donner.mod1)

donner.mod2 <- glm(survived ~ age * sex,
                   data = Donner, family = binomial)
Anova(donner.mod2)

## ----donner1-mod3--------------------------------------------------------
donner.mod3 <- glm(survived ~ poly(age, 2) + sex,
                   data = Donner, family = binomial)
donner.mod4 <- glm(survived ~ poly(age, 2) * sex,
                   data = Donner, family = binomial)
Anova(donner.mod4)

## ----donner1-summarise, R.options=list(digits=5)-------------------------
library(vcdExtra)
LRstats(donner.mod1, donner.mod2, donner.mod3, donner.mod4)

## ----donner1-LR, R.options=list(digits=4)--------------------------------
mods <- list(donner.mod1, donner.mod2, donner.mod3, donner.mod4)
LR <- sapply(mods, function(x) x$deviance)
LR <- matrix(LR, 2, 2)
rownames(LR) <- c("additive", "non-add")
colnames(LR) <- c("linear", "non-lin")
LR <- cbind(LR, diff = LR[,1] - LR[,2])
LR <- rbind(LR, diff = c(LR[1,1:2] - LR[2,1:2], NA))

## ----donner-mod5---------------------------------------------------------
library(splines)
donner.mod5 <- glm(survived ~ ns(age,2) * sex,
                   data = Donner, family = binomial)
Anova(donner.mod5)

donner.mod6 <- glm(survived ~ ns(age,4) * sex,
                   data = Donner, family = binomial)
Anova(donner.mod6)

LRstats(donner.mod4, donner.mod5, donner.mod6)

## ----donner-effect, h=5, w=8, out.width='.8\\textwidth', cap='Effect plot for the spline model \\code{donner.mod6} fit to the Donner data.', scap='Effect plot for the spline model donner.mod6 fit to the Donner data'----
library(effects)
donner.eff6 <- allEffects(donner.mod6, xlevels = list(age=seq(0, 50, 5)))
plot(donner.eff6, ticks = list(at=c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5,
                                    0.75, 0.9, 0.95, 0.99, 0.999)))


## ----arrests, child="ch07/arrests.Rnw"-----------------------------------

## ----arrests1, echo=-1---------------------------------------------------
set.seed(12345)
library(effects)
data("Arrests", package = "effects")
Arrests[sample(nrow(Arrests), 6),]

## ----arrests2------------------------------------------------------------
Arrests$year <- as.factor(Arrests$year)
arrests.mod <- glm(released ~ employed + citizen + checks
                   + colour*year + colour*age,
                   family = binomial, data = Arrests)

## ----arrests3------------------------------------------------------------
library(car)
Anova(arrests.mod)

## ----arrests4, size='footnotesize', R.options=list(digits=4)-------------
coeftest(arrests.mod)

## ----arrests-eff1, h=6, w=6, out.width='.6\\textwidth', cap='Effect plot for the main effect of skin color in the Arrests data.', fig.pos="H"----
plot(Effect("colour", arrests.mod),
     lwd = 3, ci.style = "bands", main = "",
     xlab = list("Skin color of arrestee", cex = 1.25),
     ylab = list("Probability(released)", cex = 1.25)
  )

## ----arrests-eff2, h=8, w=8, out.width='.49\\textwidth', cap='Effect plots for the interactions of color with age (left) and year (right) in the Arrests data.'----
# colour x age interaction
plot(Effect(c("colour", "age"), arrests.mod),
     lwd = 3, multiline = TRUE, ci.style = "bands", 
     xlab = list("Age", cex = 1.25),
     ylab = list("Probability(released)", cex = 1.25),
     key.args = list(x = .05, y = .99, cex = 1.2, columns = 1)
     )
# colour x year interaction
plot(Effect(c("colour", "year"), arrests.mod),
     lwd = 3, multiline = TRUE,
     xlab = list("Year", cex = 1.25),
     ylab = list("Probability(released)", cex = 1.25),
     key.args = list(x = .7, y = .99, cex = 1.2, columns = 1)
     )

## ----arrests-all, h=8, w=12, out.width='\\textwidth', cap='Effect plot for all high-order terms in the model for the Arrests data.'----
arrests.effects <- allEffects(arrests.mod,
                              xlevels = list(age = seq(15, 45, 5)))
plot(arrests.effects,
     ylab = "Probability(released)", ci.style = "bands", ask = FALSE)


## ----icu-ex, child="ch07/icu1.Rnw"---------------------------------------

## ----icu11---------------------------------------------------------------
data("ICU", package = "vcdExtra")
names(ICU)
ICU <- ICU[,-c(4, 20)]  # remove redundant race, coma

## ----icu1-full, R.options=list(digits=4)---------------------------------
icu.full <- glm(died ~ ., data = ICU, family = binomial)
summary(icu.full)

## ----icu1-lrtest---------------------------------------------------------
LRtest <- function(model)
  c(LRchisq = (model$null.deviance - model$deviance),
    df = (model$df.null - model$df.residual))

(LR <- LRtest(icu.full))
(pvalue <- 1 - pchisq(LR[1], LR[2]))

## ----icu-full1, size='footnotesize'--------------------------------------
icu.full1 <- update(icu.full, . ~ . - renal - fracture)
anova(icu.full1, icu.full, test = "Chisq")

## ----icu1-lrm1-----------------------------------------------------------
library(rms)
dd <- datadist(ICU[,-1])
options(datadist = "dd")
icu.lrm1 <- lrm(died ~ ., data = ICU)
icu.lrm1 <- update(icu.lrm1, . ~ . - renal - fracture)

## ----icu1-odds-ratios, eval=FALSE----------------------------------------
## sum.lrm1 <- summary(icu.lrm1)
## plot(sum.lrm1, log = TRUE, main = "Odds ratio for 'died'", cex = 1.25,
##      col = rgb(0.1, 0.1, 0.8, alpha = c(0.3, 0.5, 0.8)))

## ----icu1-step1----------------------------------------------------------
library(MASS)
icu.step1 <- stepAIC(icu.full1, trace = FALSE)
icu.step1$anova

## ----icu1-step2----------------------------------------------------------
icu.step2 <- stepAIC(icu.full, trace = FALSE, k = log(200))
icu.step2$anova

## ----icu1-coef-----------------------------------------------------------
coeftest(icu.step2)

## ----icu1-anova----------------------------------------------------------
anova(icu.step2, icu.step1, test = "Chisq")

## ----icu1-glm3-----------------------------------------------------------
icu.glm3 <- update(icu.step2, . ~ . - age + ns(age, 3) + 
                   (cancer + admit + uncons) ^ 2)
anova(icu.step2, icu.glm3, test = "Chisq")

## ----icu1-glm4-----------------------------------------------------------
icu.glm4 <- update(icu.step2, . ~ . + age * (cancer + admit + uncons))
anova(icu.step2, icu.glm4, test = "Chisq")

## ----lrm-nomogram, eval=FALSE--------------------------------------------
## icu.lrm2 <- lrm(died ~ age + cancer  + admit + uncons, data = ICU)
## plot(nomogram(icu.lrm2), cex.var = 1.2, lplabel = "Log odds death")

## ----icu-recode----------------------------------------------------------
levels(ICU$cancer) <- c("-", "Cancer")
levels(ICU$admit) <- c("-","Emerg")
levels(ICU$uncons) <- c("-","Uncons")

icu.glm2 <- glm(died ~ age + cancer + admit + uncons,
                data = ICU, family = binomial)

## ----icu1-binreg-plot, h=8, w=8, out.width='.65\\textwidth', cap='Fitted log odds of death in the ICU data for the model \\code{icu.glm2}. Each line shows the relationship with age, for patients having various combinations of risk factors and 1 standard error confidence bands.', scap='Fitted log odds of death in the ICU data for the model icu.glm2', fig.pos="!htb"----
binreg_plot(icu.glm2, type = "link", conf_level = 0.68,
            legend = FALSE, 
            labels = TRUE, labels_just = c("right", "bottom"),
            cex = 0, point_size = 0.8, pch = 15:17,
            ylab = "Log odds (died)",
            ylim = c(-7, 4))

## ----cleanup-rms, echo=FALSE---------------------------------------------
options(datadist = NULL)
detach(package:rms)
detach(package:Hmisc)


## ----donner2, child="ch07/donner2.Rnw"-----------------------------------

## ----donner2-inflmeasures------------------------------------------------
infl <- influence.measures(donner.mod3)
names(infl)

## ----donner2-inflmeasures2, size="scriptsize", R.options=list(width=95)----
summary(infl)

## ----donner2-inflplot, echo=-2, h=6, w=8, out.width='.75\\textwidth', cap="Influence plot (residual vs. leverage) for the Donner data model, showing Cook's D as the size of the bubble symbol. Horizontal and vertical reference lines show typical cutoff values for noteworthy residuals and leverage."----
op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
library(car)
res <- influencePlot(donner.mod3, id.col = "blue", scale = 8, id.n = 2)
k <- length(coef(donner.mod3))
n <- nrow(Donner)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)

## ----donner2-res, R.options=list(digits=4)-------------------------------
# show data together with diagnostics for influential cases
idx <- which(rownames(Donner) %in% rownames(res))
cbind(Donner[idx,2:4], res)

## ----donner2-indexinfl, h=6, w=8, out.width='.8\\textwidth', cap="Index plots of influence measures for the Donner data model. The four most extreme observations on each measure are labeled."----
influenceIndexPlot(donner.mod3, vars=c("Cook", "Studentized", "hat"), 
                   id.n=4)


## ----icu2, child="ch07/icu2.Rnw"-----------------------------------------

## ----icu2-glm2, eval=FALSE-----------------------------------------------
## icu.glm2 <- glm(died ~ age + cancer  + admit + uncons,
##                 data = ICU, family = binomial)

## ----icu2-inflplot, h=5.5, w=8, out.width='.7\\textwidth', cap='Influence plot for the main effects model for the ICU data.',fig.pos="!tb"----
library(car)
res <- influencePlot(icu.glm2, id.col = "red", 
                     scale = 8, id.cex = 1.5, id.n = 3)

## ----icu2-res, R.options=list(digits=4)----------------------------------
idx <- which(rownames(ICU) %in% rownames(res))
cbind(ICU[idx, c("died", "age", "cancer", "admit", "uncons")], res)

## ----icu2-infl-index, h=5.5, w=8, out.width='.8\\textwidth', cap="Index plots of influence measures for the ICU data model. The four most extreme observations on each measure are labeled.", fig.pos="!htb"----
influenceIndexPlot(icu.glm2, vars = c("Cook", "Studentized", "hat"), 
                   id.n = 4)

## ----icu2-dfbetas, R.options=list(digits=4)------------------------------
infl <- influence.measures(icu.glm2)
dfbetas <- data.frame(infl$infmat[,2:5])
colnames(dfbetas) <- c("dfb.age", "dfb.cancer", "dfb.admit", 
                       "dfb.uncons")
head(dfbetas)

## ----icu2-dbage, h=6, w=9, out.width='.8\\textwidth', cap='Index plot for DFBETA (Age) in the ICU data model. The observations are colored blue or red according to whether the patient lived or died.'----
op <- par(mar = c(5, 5, 1, 1) + .1)
cols <- ifelse(ICU$died == "Yes", "red", "blue")
plot(dfbetas[,1], type = "h", col = cols,
     xlab = "Observation index", 
     ylab = expression(Delta * beta[Age]), 
     cex.lab = 1.3)
points(dfbetas[,1], col = cols)
# label some points
big <- abs(dfbetas[,1]) > .25
idx <- 1 : nrow(dfbetas)
text(idx[big], dfbetas[big, 1], label = rownames(dfbetas)[big],
     cex = 0.9, pos = ifelse(dfbetas[big, 1] > 0, 3, 1), 
     xpd = TRUE)
abline(h = c(-.25, 0, .25), col = "gray")
par(op)

## ----icu2-dbscatmat, h=8, w=8, out.width='.85\\textwidth', cap= 'Scatterplot matrix for DFBETAs from the model for the ICU data. Those who lived or died are shown with blue circles and red triangles, respectively. The diagonal panels show histograms of each variable.', fig.pos='!b'----
scatterplotMatrix(dfbetas, smooth = FALSE, id.n = 2, 
  ellipse = TRUE, levels = 0.95, robust = FALSE,
  diagonal = "histogram",
  groups = ICU$died, col = c("blue", "red"))


## ----donner3-mods, eval=FALSE--------------------------------------------
## donner.mod1 <- glm(survived ~ age + sex,
##                    data = Donner, family = binomial)
## donner.mod3 <- glm(survived ~ poly(age, 2) + sex,
##                    data = Donner, family = binomial)

## ----donner-cr1, h=6, w=8, out.width='.6\\textwidth', cap='Component-plus-residual plot for the simple additive linear model, \\code{donner.mod1}. The dashed red line shows the slope of age in the full model; the smoothed green curve shows a loess fit with span = 0.5.', scap='Component-plus-residual plot for the simple additive linear model',fig.pos='htb'----
crPlots(donner.mod1, ~age, id.n=2)

## ----donner-cr2, h=6, w=8, out.width='.6\\textwidth', cap='Component-plus-residual plot for the nonlinear additive  model, \\code{donner.mod3}.', scap='Component-plus-residual plot for the nonlinear additive  model'----
crPlots(donner.mod3, ~poly(age,2), id.n=2)

## ----donner4-avp0, eval=FALSE, fig.show='hide'---------------------------
## col <- ifelse(Donner$survived == "yes", "blue", "red")
## pch <- ifelse(Donner$sex == "Male", 16, 17)
## avPlots(donner.mod1, id.n = 2,
##         col = col, pch = pch, col.lines = "darkgreen")

## ----donner4-avp, echo=FALSE, h=7, w=6, out.width='.5\\textwidth', cap='Added-variable plots for age (left) and sex (right) in the Donner Party main effects model. Those who survived are shown in blue; those who died in red. Men are plotted with filled circles; women with filled triangles. '----
col <- ifelse(Donner$survived == "yes", "blue", "red")
pch <- ifelse(Donner$sex == "Male", 16, 17)
avPlot(donner.mod1, "age", id.n = 2, pch = pch, col = col, col.lines = "darkgreen", cex.lab = 1.2)
text(30, -0.4, expression(beta[age]*" = -0.034"), pos = 4, cex = 1.25, col = "darkgreen")

avPlot(donner.mod1, "sexMale", id.n = 2, pch = pch, col = col, col.lines = "darkgreen", cex.lab = 1.2)
text(0, 0.1, expression(beta[sexMale]*" = -1.21"), pos = 4, cex = 1.25, col = "darkgreen")

## ----icu3-marginal, echo=1:5, h=6, w=8, out.width='.8\\textwidth', cap='Marginal plots of the response \\code{died} against each of the predictors in the model \\code{icu.glm2} for the \\data{ICU} data.', scap='Marginal plots of the response died against each of the predictors in the model icu.glm2 for the ICU data'----
op <- par(mfrow = c(2, 2), mar = c(4, 4, 1, 2.5) + .1, cex.lab = 1.4)
plot(died ~ age, data = ICU, col = c("lightblue", "pink"))
plot(died ~ cancer, data = ICU, col = c("lightblue", "pink"))
plot(died ~ admit, data = ICU, col = c("lightblue", "pink"))
plot(died ~ uncons, data = ICU, col = c("lightblue", "pink"))
par(op)

## ----icu3-avp1, h=6, w=6, out.width='.8\\textwidth', cap='Added-variable plots for the predictors in the model for the ICU data. Those who died and survived are shown by triangles ($\\triangle$) and circles (\\small{$\\bigcirc$}), respectively.', scap='Added-variable plots for the predictors in the model for the ICU data.', fig.pos='!htb'----
pch <- ifelse(ICU$died=="No", 1, 2)
avPlots(icu.glm2, id.n=2, pch=pch, cex.lab=1.3)

## ----icu3-glm2a----------------------------------------------------------
icu.glm2a <- glm(died ~ age + cancer  + admit + uncons + systolic,
                 data = ICU, family = binomial)
anova(icu.glm2, icu.glm2a, test = "Chisq")

## ----icu3-avp2, h=6, w=6, out.width='.6\\textwidth', cap='Added-variable plot for the effect of adding systolic blood pressure to the main effects model for the ICU data.'----
avPlot(icu.glm2a, "systolic", id.n = 3, pch = pch)

