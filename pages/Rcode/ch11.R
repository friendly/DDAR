## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch11")
.locals$ch11 <- NULL
.pkgs$ch11 <- NULL
library(ggplot2)
library(vcdExtra)
theme_set(theme_bw())  # set default ggplot theme

## ----count-data, child='ch11/count.Rnw'----------------------------------

## ----phdpubs1-1----------------------------------------------------------
data("PhdPubs", package = "vcdExtra")
with(PhdPubs, c(mean = mean(articles), var = var(articles),
                ratio = var(articles) / mean(articles)))

## ----art.tab, size='footnotesize', R.options=list(width=90)--------------
art.fac <- factor(PhdPubs$articles, levels = 0 : 19)  # include zero frequencies
art.tab <- table(art.fac)
art.tab

## ----phdpubs-barplot1, eval=FALSE----------------------------------------
## barplot(art.tab, xlab = "Number of articles", ylab = "Frequency",
##         col = "lightblue")
## abline(v = mean(PhdPubs$articles), col = "red", lwd = 3)
## ci <- mean(PhdPubs$articles) + c(-1, 1) * sd(PhdPubs$articles)
## lines(x = ci, y = c(-4, -4), col = "red", lwd = 3, xpd = TRUE)

## ----phdpubs-barplot2, eval=FALSE----------------------------------------
## barplot(art.tab + 1, ylab = "log(Frequency+1)",
##         xlab = "Number of articles", col = "lightblue", log = "y")

## ----phdpubs-logplots, h=6, w=6, out.width='.49\\textwidth', cap='Exploratory plots for the number of articles in the PhdPubs data. Left: boxplots for married (1) vs.\ non-married (0); right: jittered scatterplot vs.\ mentor publications with a lowess smoothed curve.'----
boxplot(articles + 1 ~ married, data = PhdPubs, log = "y", 
        varwidth = TRUE, ylab = "log(articles + 1)", xlab = "married", 
        cex.lab = 1.25)
plot(jitter(articles + 1) ~ mentor, data = PhdPubs, log = "y", 
     ylab="log(articles + 1)", cex.lab = 1.25)
lines(lowess(PhdPubs$mentor, PhdPubs$articles + 1), col = "blue", 
      lwd = 3)

## ----phdpubs-ggplot, eval=FALSE------------------------------------------
## ggplot(PhdPubs, aes(mentor, articles + 1)) +
##   geom_jitter(position = position_jitter(h = 0.05)) +
##   stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
##   stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
##   scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
##   labs(y = "log(articles + 1)", x = "Mentor publications")

## ----phdpubs1-factors----------------------------------------------------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)	
  married <- factor(married)
})

## ----phdpubs1-pois, size='footnotesize'----------------------------------
phd.pois <- glm(articles ~ ., data = PhdPubs, family = poisson)
summary(phd.pois)

## ----phdpubs1-coef-------------------------------------------------------
round(cbind(beta = coef(phd.pois),
            expbeta = exp(coef(phd.pois)),
            pct = 100 * (exp(coef(phd.pois)) - 1)), 3)

## ----phdpubs1-effpois, h=5, w=10, out.width='\\textwidth', cap='Effect plots for the predictors in the Poisson regression model for the PhdPubs data. Jittered values of the continuous predictors are shown at the bottom as rug-plots.'----
library(effects)
plot(allEffects(phd.pois), band.colors = "blue", lwd = 3,
     ylab = "Number of articles", main = "")

## ----phdpubs1-effpois1, eval=FALSE---------------------------------------
## plot(allEffects(phd.pois), band.colors = "blue", ylim = c(0, log(10)))


## ----crabs1, child='ch11/crabs1.Rnw'-------------------------------------

## ----crabs1-gpairs, h=8, w=8, out.width='.8\\textwidth', cap='Generalized pairs plot for the CrabSatellites data.', echo=FALSE, fig.pos="!b"----
data("CrabSatellites", package = "countreg")
library(vcd)
library(gpairs)
gpairs(CrabSatellites[, 5 : 1],
       diag.pars = list(fontsize = 16))

## ----crabs1-str, size="footnotesize"-------------------------------------
data("CrabSatellites", package = "countreg")
str(CrabSatellites)

## ----crabs1-gpairs-bis, h=8, w=8, out.width='.8\\textwidth', cap='Generalized pairs plot for the CrabSatellites data.', eval=FALSE----
## library(vcd)
## library(gpairs)
## gpairs(CrabSatellites[, 5 : 1],
##        diag.pars = list(fontsize = 16))

## ----crabs1-scats, h=5, w=6, echo=2:5, out.width='.49\\textwidth', cap='Scatterplots of number of satellites vs.\ width and weight, with lowess smooths.',size="footnotesize"----
op <- par(mar=c(4,4,1,1)+.1)
plot(jitter(satellites) ~ width, data = CrabSatellites,
  ylab = "Number of satellites (jittered)", xlab = "Carapace width",
  cex.lab = 1.25)
with(CrabSatellites, lines(lowess(width, satellites), col = "red", lwd = 2))
plot(jitter(satellites) ~ weight, data = CrabSatellites,
  ylab = "Number of satellites (jittered)", xlab = "Weight",
  cex.lab = 1.25)
with(CrabSatellites, lines(lowess(weight, satellites), col = "red", lwd = 2))
par(op)

## ----cutfac, size='footnotesize', include=FALSE--------------------------
cutfac <- function(x, breaks = NULL, q = 10) {
  if(is.null(breaks)) breaks <- unique(quantile(x, (0 : q) / q))
  x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
  levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
    c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""), sep = "")
  return(x)
}

## ----crabs1-boxplots, h=4, w=5, echo=2:3, out.width='.49\\textwidth', cap='Boxplots of number of satellites vs.\ width and weight.'----
op <- par(mar=c(4,4,1,1)+.1)
plot(satellites ~ cutfac(width), data = CrabSatellites,
     ylab = "Number of satellites", xlab = "Carapace width (deciles)")
plot(satellites ~ cutfac(weight), data = CrabSatellites,
     ylab = "Number of satellites", xlab = "Weight (deciles)")
par(op)

## ----crabs1-pois6--------------------------------------------------------
crabs.pois <- glm(satellites ~ ., data = CrabSatellites, 
                                  family = poisson)
summary(crabs.pois)

## ----crabs1-eff1, h=6, w=9, out.width='\\textwidth', cap='Effect plots for the predictors in the Poisson regression model for the CrabSatellites data.'----
plot(allEffects(crabs.pois), main = "")

## ----crabs1-pois1, size='footnotesize'-----------------------------------
CrabSatellites1 <- transform(CrabSatellites, color = as.numeric(color))

crabs.pois1 <- glm(satellites ~ weight + color, data = CrabSatellites1,
                   family = poisson)
summary(crabs.pois1)

## ------------------------------------------------------------------------
LRstats(crabs.pois, crabs.pois1)


## ----phdpubs2-phi--------------------------------------------------------
with(phd.pois, deviance / df.residual)
sum(residuals(phd.pois, type = "pearson")^2) / phd.pois$df.residual

## ----phdpubs2-quasi------------------------------------------------------
phd.qpois <- glm(articles ~ ., data = PhdPubs, family = quasipoisson)

## ----phdpubs2-phi2-------------------------------------------------------
(phi <- summary(phd.qpois)$dispersion)

## ----crabs-nbin----------------------------------------------------------
library(MASS)
crabs.nbin <- glm.nb(satellites ~ weight + color, 
                     data = CrabSatellites1)
crabs.nbin$theta

## ----crabs-nbin1---------------------------------------------------------
crabs.nbin1 <- glm(satellites ~ weight + color, data = CrabSatellites1,
                   family = negative.binomial(1))

## ----phdpubs-nbin, echo=FALSE--------------------------------------------
library(MASS)
phd.nbin  <- glm.nb(articles ~ ., data = PhdPubs)

## ----phdpubs3-fitted-----------------------------------------------------
fit.pois <- fitted(phd.pois, type = "response")
fit.nbin <- fitted(phd.nbin, type = "response")

## ----cutq----------------------------------------------------------------
cutq <- function(x, q = 10) {
    quantile <- cut(x, breaks = quantile(x, probs = (0 : q) / q),
        include.lowest = TRUE, labels = 1 : q)
    quantile
}


## ----qdat1---------------------------------------------------------------
group <- cutq(fit.nbin, q = 20)
qdat <- aggregate(PhdPubs$articles,
          list(group),
          FUN = function(x) c(mean = mean(x), var = var(x)))
qdat <- data.frame(qdat$x)
qdat <- qdat[order(qdat$mean),]

## ----qdat2---------------------------------------------------------------
phi <- summary(phd.qpois)$dispersion
qdat$qvar <- phi * qdat$mean
qdat$nbvar <- qdat$mean + (qdat$mean^2) / phd.nbin$theta
head(qdat)

## ----phd-mean-var-plot, h=6, w=9, out.width='.75\\textwidth', cap='Mean--variance functions for the PhdPubs data. Points show the observed means and variances for 20 quantile groups based on the fitted values in the negative-binomial model. The labeled lines and curves show the variance functions implied by various models.'----
with(qdat, {
  plot(var ~ mean, xlab = "Mean number of articles", ylab = "Variance",
       pch = 16, cex = 1.2, cex.lab = 1.2)
  abline(h = mean(PhdPubs$articles), col = gray(.40), lty = "dotted")
  lines(mean, qvar, col = "red", lwd = 2)
  lines(mean, nbvar, col = "blue", lwd = 2)
  lines(lowess(mean, var), lwd = 2, lty = "dashed")
  text(3, mean(PhdPubs$articles), "Poisson", col = gray(.40))
  text(3, 5, "quasi-Poisson", col = "red")
  text(3, 6.7, "negbin", col = "blue")
  text(3, 8.5, "lowess")
})

## ----phdpubs3-SE---------------------------------------------------------
library(sandwich)
phd.SE <- sqrt(cbind(
  pois = diag(vcov(phd.pois)),
  sand = diag(sandwich(phd.pois)),
  qpois = diag(vcov(phd.qpois)),
  nbin = diag(vcov(phd.nbin))))
round(phd.SE, 4)

## ----phd-disptest--------------------------------------------------------
library(AER)
dispersiontest(phd.pois)
dispersiontest(phd.pois, 2)

## ----phdpubs4-rootogram, w=6, h=4, out.width='.49\\textwidth', cap='Hanging rootograms for the PhdPubs data.'----
library(countreg)
countreg::rootogram(phd.pois, max = 12, 
                    main = "PhDPubs: Poisson")
countreg::rootogram(phd.nbin, max = 12, 
                    main = "PhDPubs: Negative-Binomial")

## ----crabs2-rootogram, w=6, h=4, out.width='.49\\textwidth', cap='Hanging rootograms for the CrabSatellites data.'----
countreg::rootogram(crabs.pois, max = 15, 
                    main = "CrabSatellites: Poisson")
countreg::rootogram(crabs.nbin, max = 15, 
                    main = "CrabSatellites: Negative-Binomial")

## ----zeros, child='ch11/zeros.Rnw'---------------------------------------

## ----zipois1-------------------------------------------------------------
library(VGAM)
set.seed(1234)
data1 <- rzipois(200, 3, 0)
data2 <- rzipois(200, 3, .3)

## ----zipois-plot, h=6, w=6, out.width='.49\\textwidth', cap='Bar plots of simulated data from Poisson and zero-inflated Poisson distributions.'----
tdata1 <- table(data1)
barplot(tdata1, xlab = "Count", ylab = "Frequency",
        main = "Poisson(3)")
tdata2 <- table(data2)
barplot(tdata2, xlab = "Count", ylab = "Frequency",
        main = expression("ZI Poisson(3, " * pi * "= .3)"))

## ----detach-vgam, echo=FALSE---------------------------------------------
detach(package:VGAM)

## ----crabs-fix-color, include=FALSE--------------------------------------
CrabSatellites <- transform(CrabSatellites,
  color = as.numeric(color))

## ----crabs-zero-spinogram, h=4, w=10, echo=2:3, out.width='\\textwidth', cap='Spinograms for the CrabSatellites data. The variables weight (left) and color (right) have been made discrete using quantiles of their distributions.'----
op <- par(cex.lab=1.2, mfrow = c(1, 2))
plot(factor(satellites == 0) ~ weight, data = CrabSatellites,
     breaks = quantile(weight, probs = seq(0,1,.2)), ylevels = 2:1,
     ylab = "No satellites")
plot(factor(satellites == 0) ~ color, data = CrabSatellites,
     breaks = quantile(color, probs = seq(0,1,.33)),  ylevels = 2:1,
     ylab = "No satellites")
par(op)

## ----crabs-zero-cdplot, h=4, w=10, echo=2:3, out.width='\\textwidth', cap='Conditional density plots for the CrabSatellites data. The region shaded below shows the conditional probability density estimate for a count of zero.'----
op <- par(cex.lab = 1.2, mfrow = c(1, 2))
cdplot(factor(satellites == 0) ~ weight, data = CrabSatellites,
       ylevels = 2:1, ylab = "No satellites")
cdplot(factor(satellites == 0) ~ color, data = CrabSatellites,
       ylevels = 2:1, , ylab = "No satellites")
par(op)


## ----cod1, child='ch11/cod1.Rnw'-----------------------------------------

## ----cod-prevalence, eval=FALSE------------------------------------------
## CodParasites$prevalence <-
##     ifelse(CodParasites$intensity == 0, "no", "yes")

## ----cod1-1, size='footnotesize', R.options=list(width=80)---------------
data("CodParasites", package = "countreg")
summary(CodParasites[, c(1 : 4, 7)])

## ----cod1-gpairs, h=8, w=8, out.width='.8\\textwidth', cap='Generalized pairs plot for the CodParasites data.', fig.pos='htb!'----
library(vcd)
library(gpairs)
gpairs(CodParasites[, c(1 :4, 7)],
       diag.pars = list(fontsize = 16),
       mosaic.pars = list(gp = shading_Friendly))

## ----cod1-tab------------------------------------------------------------
cp.tab <- xtabs(~ area + year + factor(is.na(prevalence) |
                                       prevalence == "yes"),
                data = CodParasites)
dimnames(cp.tab)[3] <- list(c("No", "Yes"))
names(dimnames(cp.tab))[3] <- "prevalence"

## ----cod1-doubledecker, h=5, w=10, out.width='.9\\textwidth', cap='Doubledecker plot for prevalence against area and year in the CodParasites data. The cases of infected fish are highlighted.'----
doubledecker(prevalence ~ area + year, data = cp.tab,
             margins = c(1, 5, 3, 1))

## ----cod1-mosaic, h=5, w=10, out.width='.9\\textwidth', cap='Mosaic plot for prevalence against area and year in the CodParasites data, in the doubledecker format. Shading reflects departure from a model in which prevalence is independent of area and year jointly.'----
doubledecker(prevalence ~ area + year, data = cp.tab,
             gp = shading_hcl, expected = ~ year:area + prevalence,
	     margins = c(1, 5, 3, 1))

## ----cod1-length-prevalence, h=5, w=7, out.width='.6\\textwidth', cap='Jittered scatterplot of prevalence against length of fish, with loess smooth.'----
library(ggplot2)
ggplot(CodParasites, aes(x = length, y = as.numeric(prevalence) - 1)) +
  geom_jitter(position = position_jitter(height = .05), alpha = 0.25) +
  geom_rug(position = "jitter", sides = "b") +
  stat_smooth(method = "loess", color = "red", 
              fill = "red", size = 1.5) +
  labs(y = "prevalence")

## ----cod1-boxplot-bis, h=5, w=10, out.width='.9\\textwidth', cap='Notched boxplots for log (intensity) of parasites by area and year in the CodParasites data. Significant differences in the medians are signaled when the notches of two groups do not overlap.', eval=FALSE----
## # plot only positive values of intensity
## CPpos <- subset(CodParasites, intensity > 0)
## ggplot(CPpos, aes(x = year, y = intensity)) +
##   geom_boxplot(outlier.size = 3, notch = TRUE, aes(fill = year),
##                alpha = 0.2) +
##   geom_jitter(position = position_jitter(width = 0.1), alpha = 0.25) +
##   facet_grid(. ~ area) +
##   scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200)) +
##   theme(legend.position = "none") +
##   labs(y = "intensity (log scale)")

## ----CPpos-kludge, echo=FALSE--------------------------------------------
CPpos <- subset(CodParasites, intensity > 0)

## ----cod1-length-scat, h=5, w=7, out.width='.6\\textwidth', cap='Jittered scatterplot of log (intensity) for the positive counts against length of fish, with loess smooth and linear regression line.'----
ggplot(CPpos, aes(x = length, y = intensity)) +
  geom_jitter(position = position_jitter(height = .1), alpha = 0.25) +
  geom_rug(position = "jitter", sides = "b") +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  stat_smooth(method = "loess", color = "red", fill = "red", size = 2) +
  stat_smooth(method = "lm", size = 1.5)

## ----cod2-mod1-----------------------------------------------------------
library(MASS)
library(countreg)
cp_p  <- glm(intensity ~ length + area * year,
             data = CodParasites, family = poisson)
cp_nb <- glm.nb(intensity ~ length + area * year,
                data = CodParasites)

## ----cod2-mod2-----------------------------------------------------------
cp_hp  <- hurdle(intensity ~ length + area * year,
                 data = CodParasites, dist = "poisson")
cp_hnb <- hurdle(intensity ~ length + area * year,
                 data = CodParasites, dist = "negbin")
cp_zip <- zeroinfl(intensity ~ length + area * year,
                   data = CodParasites, dist = "poisson")
cp_znb <- zeroinfl(intensity ~ length + area * year,
                   data = CodParasites, dist = "negbin")

## ----cod2-rootograms, h=8, w=6, echo=2:7, out.width='.8\\textwidth', cap='Rootograms for six models fit to the CodParasites data.', fig.pos="!t"----
op <- par(mfrow = c(3, 2))
countreg::rootogram(cp_p, max = 50, main = "Poisson")
countreg::rootogram(cp_nb, max = 50, main = "Negative Binomial")
countreg::rootogram(cp_hp, max = 50, main = "Hurdle Poisson")
countreg::rootogram(cp_hnb, max = 50, main = "Hurdle Negative Binomial")
countreg::rootogram(cp_zip, max = 50, main = "Zero-inflated Poisson")
countreg::rootogram(cp_znb, max = 50, 
                            main = "Zero-inflated Negative Binomial")
par(op)

## ----cod2-summarise------------------------------------------------------
LRstats(cp_p, cp_nb, cp_hp, cp_hnb, cp_zip, cp_znb, sortby = "BIC")

## ----cod2-lrtest---------------------------------------------------------
library(lmtest)
lrtest(cp_hp, cp_hnb)

## ----cod2-vuong----------------------------------------------------------
library(pscl)
vuong(cp_nb, cp_hnb)     # nb vs. hurdle nb
vuong(cp_hnb, cp_znb)    # hurdle nb vs znb

## ----cod2-summary, size='footnotesize', R.options=list(width=80)---------
summary(cp_hnb)

## ----cod2-hnb1-----------------------------------------------------------
cp_hnb1 <- hurdle(intensity ~ length + area * year | area * year,
                  data = CodParasites, dist = "negbin")

## ------------------------------------------------------------------------
lrtest(cp_hnb, cp_hnb1)
vuong(cp_hnb, cp_hnb1)

## ----detach, echo=FALSE--------------------------------------------------
detach(package:pscl)

## ----cod3-eff1, h=6, w=6, out.width='.49\\textwidth', cap='Effect plots for total intensity of parasites from the negative-binomial model.'----
library(effects)
eff.nb <- allEffects(cp_nb)
plot(eff.nb[1], type = "response", ylim = c(0,30),
     main  ="NB model: length effect")

plot(eff.nb[2], type = "response", ylim = c(0,30),
     multiline = TRUE, ci.style = "bars",
     key.args = list(x = .05, y = .95, columns = 1),
     colors = c("black", "red", "blue") ,
     symbols = 15 : 17, cex = 2,
     main = "NB model: area*year effect")

## ------------------------------------------------------------------------
cp_zero  <- glm(prevalence ~ length + area * year,
                data = CodParasites, family = binomial)
cp_nzero <- glm.nb(intensity ~ length + area * year,
                   data = CodParasites, subset = intensity > 0)

## ----cod3-eff2, h=6, w=6, out.width='.49\\textwidth', cap='Effect plots for prevalence of parasites analogous to  the hurdle negative-binomial model, fitted using a binomial GLM model.'----
eff.zero <- allEffects(cp_zero)
plot(eff.zero[1], ylim=c(-2.5, 2.5),
     main="Hurdle zero model: length effect")

plot(eff.zero[2],  ylim=c(-2.5, 2.5),
     multiline=TRUE,
     key.args=list(x=.05, y=.95, columns=1),
     colors=c("black", "red", "blue"),
     symbols=15:17, cex=2,
     main="Hurdle zero model: area*year effect")


## ----nmes1, child='ch11/nmes1.Rnw'---------------------------------------

## ----nmes1-data----------------------------------------------------------
data("NMES1988", package = "AER")
nmes <- NMES1988[, c(1, 6:8, 13, 15, 18)]

## ----nmes-visits, h=6, w=6, out.width='.49\\textwidth', cap='Frequency distributions of the number of physician office visits.'----
plot(table(nmes$visits),
     xlab = "Physician office visits", ylab = "Frequency")
plot(log(table(nmes$visits)),
     xlab = "Physician office visits", ylab = "log(Frequency)")

## ----nmes-mean-var-------------------------------------------------------
with(nmes, c(mean = mean(visits),
             var = var(visits),
             ratio = var(visits) / mean(visits)))

## ----nmes-boxplots, h=4, w=9, echo=2:4, out.width='\\textwidth', cap='Number of physician office visits plotted against some of the predictors.'----
op <-par(mfrow=c(1, 3), cex.lab=1.4)
plot(log(visits + 1) ~ cutfac(chronic), data = nmes,
     ylab = "Physician office visits (log scale)",
     xlab = "Number of chronic conditions", main = "chronic")
plot(log(visits + 1) ~ health, data = nmes, varwidth = TRUE,
     ylab = "Physician office visits (log scale)",
     xlab = "Self-perceived health status", main = "health")
plot(log(visits + 1) ~ cutfac(hospital, c(0:2, 8)), data = nmes,
     ylab = "Physician office visits (log scale)",
     xlab = "Number of hospital stays", main = "hospital")
par(op)

## ----nmes-school, h=5, w=6, out.width='.6\\textwidth', cap='Jittered scatterplot of physician office visits against number of years of education, with nonparametric (loess) smooth.'----
library(ggplot2)
ggplot(nmes, aes(x = school, y = visits + 1)) +
  geom_jitter(alpha = 0.25) +
  stat_smooth(method = "loess", color = "red", fill = "red", 
              size = 1.5, alpha = 0.3) +
  labs(x = "Number of years of education", 
       y = "log(Physician office visits + 1)") +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100))

## ----nmes2-initial-------------------------------------------------------
nmes.pois <-    glm(visits ~ ., data = nmes, family = poisson)
nmes.nbin <- glm.nb(visits ~ ., data = nmes)

## ----nmes2-lrtest1-------------------------------------------------------
library(lmtest)
lrtest(nmes.pois, nmes.nbin)

## ----nmes2-nbin-summary--------------------------------------------------
summary(nmes.nbin)

## ----nmes2-add1----------------------------------------------------------
add1(nmes.nbin, . ~ .^2, test = "Chisq")

## ----nmes2-nbin2---------------------------------------------------------
nmes.nbin2 <- update(nmes.nbin,
                     . ~ . + (health + chronic + hospital)^2
                           + health : school)

## ----nmes2-lrtest2-------------------------------------------------------
lrtest(nmes.nbin, nmes.nbin2)

## ----nmes2-eff1, h=5, w=9, out.width='\\textwidth', cap='Effect plots for the main effects of each predictor in the negative binomial model nmes.nbin.'----
library(effects)
plot(allEffects(nmes.nbin), ylab = "Office visits")

## ----eff-nbin2-----------------------------------------------------------
eff_nbin2 <- allEffects(nmes.nbin2,
  xlevels = list(hospital = c(0 : 3, 6, 8), 
  chronic = c(0:3, 6, 8), 
  school = seq(0, 20, 5)))

## ------------------------------------------------------------------------
names(eff_nbin2)

## ----nmes2-eff2, h=5, w=12, out.width='\\textwidth', cap='Effect plot for the interaction of health and number of chronic conditions in the model nmes.nbin2.'----
plot(eff_nbin2, "health:chronic", layout = c(3, 1),
     ylab = "Office visits", colors = "blue")

## ----nmes2-eff3, h=6, w=6, out.width='.49\\textwidth', cap='Effect plots for the interactions of chronic conditions and hospital stays with perceived health status in the model nmes.nbin2.'----
plot(eff_nbin2, 
     "health:chronic", multiline = TRUE, ci.style = "bands",
     ylab = "Office visits", xlab  ="# Chronic conditions",
     key.args = list(x = 0.05, y = .80, corner = c(0, 0), columns = 1))
	
plot(eff_nbin2, 
     "hospital:health", multiline = TRUE, ci.style = "bands",
     ylab = "Office visits", xlab = "Hospital stays",
     key.args = list(x = 0.05, y = .80, corner = c(0, 0), columns = 1))

## ----nmes2-eff4, h=6, w=6, out.width='.49\\textwidth', cap='Effect plots for the interactions of chronic conditions and hospital stays and for health status with years of education in the model nmes.nbin2.'----
plot(eff_nbin2, "hospital:chronic", multiline = TRUE, ci.style = "bands",
     ylab = "Office visits", xlab = "Hospital stays",
     key.args = list(x = 0.05, y = .70, corner = c(0, 0), columns = 1))

plot(eff_nbin2, "health:school", multiline = TRUE,  ci.style = "bands",
     ylab = "Office visits", xlab = "Years of education",
     key.args = list(x = 0.65, y = .1, corner = c(0, 0), columns = 1))

## ----nmes3-nbin3---------------------------------------------------------
nmes.nbin3 <- update(nmes.nbin2, . ~ . + I(chronic^2) + I(hospital^2))

## ----nmes3-nbin3a, eval=FALSE--------------------------------------------
## nmes.nbin3 <- glm.nb(visits ~ poly(hospital, 2) + poly(chronic, 2) +
##                      insurance + school + gender +
##                      (health + chronic + hospital)^2 + health : school,
## 		     data = nmes)

## ----fm_anova------------------------------------------------------------
ret <- anova(nmes.nbin, nmes.nbin2, nmes.nbin3)
ret$Model <- c("nmes.nbin", "nmes.nbin2", "nmes.nbin3")
ret
LRstats(nmes.nbin, nmes.nbin2, nmes.nbin3)

## ----nmes3-eff1, h=5, w=12, out.width='\\textwidth', cap='Effect plot for the interaction of health and number of chronic conditions in the quadratic model nmes.nbin3.'----
eff_nbin3 <- allEffects(nmes.nbin3,
  xlevels = list(hospital = c(0 : 3, 6, 8), 
                 chronic = c (0 : 3, 6, 8), 
		 school = seq(0, 20, 5)))
plot(eff_nbin3, "health : chronic", layout = c(3, 1))

## ----nmes3-gamnb---------------------------------------------------------
library(mgcv)
nmes.gamnb <- gam(visits ~ s(hospital, k = 3) + s(chronic, k = 3) +
                           insurance + school + gender +
                           (health + chronic + hospital)^2 + 
			   health : school,
                  data = nmes, family = nb())

## ----nmes3-rsm, h=6, w=6, out.width='.5\\textwidth', cap='Fitted response surfaces for the relationships among chronic conditions,  number of hospital stays, and years of education to office visits in the generalized additive model, nmes.gamnb.', echo=FALSE, fig.pos="hbt"----
library(rsm)
library(colorspace)
persp(nmes.gamnb, hospital ~ chronic, zlab = "log Office visits",
  col = rainbow_hcl(30), contour = list(col = "colors", lwd = 2),
  at = list(school = 10, health = "average"), theta = -60)

persp(nmes.gamnb, school ~ chronic, zlab = "log Office visits",
  col = rainbow_hcl(30), 
  contour = list(col = "colors", lwd = 2, z = "top"),
  at = list(hospital = 0.3, health = "average"), theta = -60)

## ----nmes3-rsm-bis, h=6, w=6, out.width='.5\\textwidth', cap='Fitted response surfaces for the relationships among chronic conditions,  number of hospital stays, and years of education to office visits in the generalized additive model, nmes.gamnb.', eval=FALSE----
## library(rsm)
## library(colorspace)
## persp(nmes.gamnb, hospital ~ chronic, zlab = "log Office visits",
##   col = rainbow_hcl(30), contour = list(col = "colors", lwd = 2),
##   at = list(school = 10, health = "average"), theta = -60)
## 
## persp(nmes.gamnb, school ~ chronic, zlab = "log Office visits",
##   col = rainbow_hcl(30),
##   contour = list(col = "colors", lwd = 2, z = "top"),
##   at = list(hospital = 0.3, health = "average"), theta = -60)


## ----phdpubs5-plot, h=8, w=8, echo=2, out.width='.7\\textwidth', cap='Default diagnostic plots for the negative-binomial model fit to the PhdPubs data.'----
op <- par(mfrow=c(2,2), mar=c(4,4,2,1)+.1, cex.lab=1.2)
plot(phd.nbin)
par(op)

## ----phdpubs5-resplot1, h=6, w=6, out.width='.5\\textwidth', cap='Plots of residuals against the linear predictor using residualPlot(). The right panel shows that the diagonal bands correspond to different values of the discrete response.'----
library(car)
residualPlot(phd.nbin, type = "rstandard", col.smooth = "red", id.n = 3)
residualPlot(phd.nbin, type = "rstandard",
             groups = PhdPubs$articles, key = FALSE, linear = FALSE, 
	     smoother = NULL)

## ----phdpubs5-resplot2, h=6, w=6, out.width='.5\\textwidth', cap='Plots of residuals against two predictors in the phd.nbin model. Such plots should show no evidence of a systematic trend for a good-fitting model.'----
residualPlot(phd.nbin, "mentor", type = "rstudent",
             quadratic = TRUE, col.smooth = "red", col.quad = "blue", 
	     id.n = 3)
residualPlot(phd.nbin, "phdprestige", type = "rstudent",
             quadratic = TRUE, col.smooth = "red", col.quad = "blue", 
	     id.n = 3)

## ----phdpubs5-influenceplot, h=6, w=8, out.width='.7\\textwidth', cap="Influence plot showing leverage, studentized residuals, and Cook's distances for the negative-binomial model fit to the PhdPubs data. Conventional cutoffs for studentized residuals are shown by dashed horizontal lines at $\\pm 2$; vertical lines show 2 and 3 times the average hat-value."----
influencePlot(phd.nbin)

## ----phdpubs-outlierTest-------------------------------------------------
outlierTest(phd.nbin)

## ----phdpubs6-qqplot, h=6, w=6, out.width='.5\\textwidth', cap='Normal QQ plot of the studentized residuals from the NB model for the PhdPubs data. The normal-theory reference line and confidence envelope are misleading here.'----
qqPlot(rstudent(phd.nbin), id.n = 3,
       xlab = "Normal quantiles", ylab = "Studentized residuals")

## ----phdpubs6-obs, eval=FALSE--------------------------------------------
## observed <- sort(abs(rstudent(phd.nbin)))
## n <- length(observed)
## expected <- qnorm((1:n + n - 1/8) / (2*n + 1/2))

## ----phdpubs6-sims, eval=FALSE-------------------------------------------
## S <- 100
## sims <- simulate(phd.nbin, nsim = S)
## simdat <- cbind(PhdPubs, sims)

## ----phdpubs6-simres, eval=FALSE-----------------------------------------
## # calculate residuals for one simulated data set
## resids <- function(y)
##   rstudent(glm.nb(y ~ female + married + kid5 + phdprestige + mentor,
##                   data=simdat, start=coef(phd.nbin)))
## # do them all ...
## simres <- matrix(0, nrow(simdat), S)
## for(i in 1:S) {
## 	simres[,i] <- sort(abs(resids(dat[,paste("sim", i, sep="_")])))
## }

## ----phdpubs6-env, eval=FALSE--------------------------------------------
## envelope <- 0.95
## mean <- apply(simres, 1, mean)
## lower <- apply(simres, 1, quantile, prob = (1 - envelope) / 2)
## upper <- apply(simres, 1, quantile, prob = (1 + envelope) / 2)

## ----phdpubs6-hnp, eval=FALSE--------------------------------------------
## plot(expected, observed,
##      xlab = "Expected value of half-normal order statistic",
##      ylab = "Absolute value of studentized residual")
## lines(expected, mean, lty = 1, lwd = 2, col = "blue")
## lines(expected, lower, lty = 2, lwd = 2, col = "red")
## lines(expected, upper, lty = 2, lwd = 2, col = "red")
## identify(expected, observed, labels = names(observed), n = 3)

## ----phdpubs6-res-plots, h=6, w=6, out.width='.5\\textwidth', cap='Further plots of studentized residuals. Left: density plot; right: residuals against log(articles+1).'----
# examine distribution of residuals
res <- rstudent(phd.nbin)
plot(density(res), lwd = 2, col = "blue",
     main = "Density of studentized residuals")
rug(res)

# why the bimodality?
plot(jitter(log(PhdPubs$articles + 1), factor = 1.5), res,
     xlab = "log (articles + 1)", ylab = "Studentized residual")

## ----multiv, child='ch11/multiv.Rnw'-------------------------------------



## ----nmes-multiv, child='ch11/nmes-multiv.Rnw'---------------------------

## ----nmes4-data----------------------------------------------------------
data("NMES1988", package = "AER")
nmes2 <- NMES1988[, c(1 : 4, 6 : 8, 13, 15, 18)]
names(nmes2)[1 : 4]     # responses
names(nmes2)[-(1 : 4)]  # predictors

## ----nmes4-mlm-----------------------------------------------------------
clog <- function(x) log(x + 1)
nmes.mlm <- lm(clog(cbind(visits, nvisits, ovisits, novisits)) ~ .,
               data = nmes2)

## ----nmes4-hepairs, h=8, w=8, out.width='.8\\textwidth', cap='Pairwise HE plots for all responses in the nmes2 data.', fig.pos='!htb'----
library(heplots)
vlabels <- c("Physician\noffice visits", 
             "Non-physician\n office visits",
             "Physician\nhospital visits", 
	     "Non-physician\nhospital visits")
pairs(nmes.mlm, factor.means = "health", 
      fill = TRUE, var.labels = vlabels)

## ----nmes4-reshape-------------------------------------------------------
vars <- colnames(nmes2)[1 : 4]
nmes.long <- reshape(nmes2,
  varying = vars,
  v.names = "visit",
  timevar = "type",
  times = vars,
  direction = "long",
  new.row.names = 1 : (4 * nrow(nmes2)))

## ----nmes4-long, size='footnotesize'-------------------------------------
nmes.long <- nmes.long[order(nmes.long$id),]
nmes.long <- transform(nmes.long,
  practitioner = ifelse(type %in% c("visits", "ovisits"),
                        "physician", "nonphysician"),
  place = ifelse(type %in% c("visits", "nvisits"), "office", "hospital"),
  hospf = cutfac(hospital, c(0 : 2, 8)),
  chronicf = cutfac(chronic))

## ------------------------------------------------------------------------
xtabs(visit ~ practitioner + place, data = nmes.long)

## ----nmes4-fourfold1, h=6, w=6, out.extra='trim=0 130 0 130,clip', out.width='\\textwidth', cap='Fourfold displays for the association between practitioner and place in the nmes.long data, conditioned on health status.'----
library(vcdExtra)
fourfold(xtabs(visit ~ practitioner + place + health, data = nmes.long),
         mfrow=c(1,3))
loddsratio(xtabs(visit ~ practitioner + place + health, 
                 data = nmes.long))

## ----nmes4-fourfold2, h=8, w=8, out.width='\\textwidth', cap='Fourfold displays for the association between practitioner and place in the nmes.long data, conditioned on gender, insurance, and number of chronic conditions. Rows are levels of chronic; columns are the combinations of gender and insurance.', fig.pos='!htb'----
tab <- xtabs(visit ~ practitioner + place + gender + 
                     insurance + chronicf,
             data = nmes.long)
fourfold(tab, mfcol=c(4,4), varnames=FALSE)

## ----nmes4-loddsratio, h=5, w=9, out.width='.85\\textwidth', cap='Plot of log odds ratios with 1 standard error bars for the association between practitioner and place, conditioned on gender, insurance, and number of chronic conditions. The horizontal lines show the null model (longdash) and the mean (dot--dash) of the log odds ratios.'----
lodds.df <- as.data.frame(loddsratio(tab))
library(ggplot2)
ggplot(lodds.df, aes(x = chronicf, y = LOR,
                     ymin = LOR - 1.96 * ASE, ymax = LOR + 1.96 * ASE,
                     group = insurance, color = insurance)) +
  geom_line(size = 1.2) + geom_point(size = 3) +
  geom_linerange(size = 1.2) +
  geom_errorbar(width = 0.2) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = mean(lodds.df$LOR), linetype = "dotdash") +
  facet_grid(. ~ gender, labeller = label_both) +
  labs(x = "Number of chronic conditions",
       y = "log odds ratio (physician|place)") +
  theme_bw() + theme(legend.position = c(0.1, 0.9)) 

## ----nmes4-lodds-mod-----------------------------------------------------
lodds.mod <- lm(LOR ~ (gender + insurance + chronicf)^2,
                weights = 1 / ASE^2, data = lodds.df)
anova(lodds.mod)

## ----load-VGAM, echo=FALSE-----------------------------------------------
library(VGAM)

## ----nmes5-nbin, cache=TRUE----------------------------------------------
nmes2.nbin <- vglm(cbind(visits, nvisits, ovisits, novisits) ~ .,
                   data = nmes2, family = negbinomial)

## ----nmes5-coef1---------------------------------------------------------
# coefficients for visits
coef(nmes2.nbin, matrix = TRUE)[,c(1, 2)]
# theta for visits
exp(coef(nmes2.nbin, matrix = TRUE)[1, 2])

## ----nmes5-coef2---------------------------------------------------------
coef(nmes2.nbin, matrix = TRUE)[,c(1, 3, 5, 7)]

## ----nmes5-clist---------------------------------------------------------
clist <- constraints(nmes2.nbin, type = "term")
clist$hospital[c(1, 3, 5, 7),]

## ----nmes5-clist2--------------------------------------------------------
clist2 <- clist
clist2$hospital <- cbind(rowSums(clist$hospital))
clist2$chronic  <- cbind(rowSums(clist$chronic))
clist2$hospital[c(1, 3, 5, 7), 1, drop = FALSE]

## ----nmes5-nbin2, cache=TRUE---------------------------------------------
nmes2.nbin2 <- vglm(cbind(visits, nvisits, ovisits, novisits) ~ ., 
                    data = nmes2, constraints = clist2,
                    family = negbinomial(zero = NULL))

## ----nmes5-coef3---------------------------------------------------------
coef(nmes2.nbin2, matrix = TRUE)[,c(1, 3, 5, 7)]

## ----nmes5-lrtest--------------------------------------------------------
lrtest(nmes2.nbin, nmes2.nbin2)

## ----nmes5-linhyp1-------------------------------------------------------
lh <- paste("hospital:", 1 : 3, " = ", "hospital:", 2 : 4, sep="")
lh

## ----nmes5-foxkludge, include=FALSE--------------------------------------
if (packageVersion("car") < "2.1.1") {
	df.residual.vglm <- function(object, ...) object@df.residual
	vcov.vglm <- function(object, ...) vcovvlm(object, ...)
	coef.vglm <- function(object, ...) coefvlm(object, ...)
}

## ----nmes5-linhyp2-------------------------------------------------------
car::linearHypothesis(nmes2.nbin, lh)


