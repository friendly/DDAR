## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch03", fig.pos = 'tb')
require(vcdExtra, quietly = TRUE, warn.conflicts = FALSE)
.locals$ch03 <- NULL
.pkgs$ch03 <- NULL

## ----arbuthnot1, echo=-1, h=4, w=7, out.width=".75\\textwidth", fig.pos='t', cap="Arbuthnot's data on male/female sex ratios in London, 1629--1710, together with a (loess) smoothed curve (blue) over time and the mean Pr(Male).", scap="Arbuthnot's data on male/female sex ratios"----
op <- par(mar = c(4,4,1,1) + .1, cex.lab = 1.25)
data("Arbuthnot", package = "HistData")
with(Arbuthnot, {
  prob = Males / (Males + Females)
  plot(x = Year, y = prob, type = "b",
       ylim = c(0.5, 0.54), ylab = "Pr (Male)")
  abline(h = 0.5, col = "red", lwd = 2)
  abline(h = mean(prob), col = "blue")
  lines(loess.smooth(Year, prob), col = "blue", lwd = 2)
  text(x = 1640, y = 0.5, expression(H[0]: "Pr(Male)=0.5"),
       pos = 3, col = "red")
  })

## ----saxony-barplot, echo=-1, w=8, h=4, out.width=".75\\textwidth", cap="Number of males in Saxony families of size 12."----
spar()
data("Saxony", package = "vcd")
barplot(Saxony, xlab = "Number of males", ylab = "Number of families",
        col = "lightblue", cex.lab = 1.5)

## ----dice, w=8, h=4, out.width=".75\\textwidth", cap="Weldon's dice data, frequency distribution of 5s and 6s in throws of 12 dice."----
data("WeldonDice", package = "vcd")
dimnames(WeldonDice)$n56[11] <- "10+"
barplot(WeldonDice, xlab = "Number of 5s and 6s", ylab = "Frequency",
        col = "lightblue", cex.lab = 1.5)

## ----horsekicks, w=8, h=4, out.width=".75\\textwidth", cap="HorseKicks data, distribution of the number of deaths in 200 corps-years."----
data("HorseKicks", package = "vcd")
barplot(HorseKicks, xlab = "Number of deaths", ylab = "Frequency",
        col = "lightblue", cex.lab = 1.5)


## ----federalist, w=8, h=4, out.width=".75\\textwidth", cap="Federalist Papers data, distribution of the uses of the word \\emph{may}.", scap="Federalist Papers data"----
data("Federalist", package = "vcd")
barplot(Federalist,
        xlab = "Occurrences of 'may'",
        ylab = "Number of blocks of text",
        col = "lightgreen", cex.lab = 1.5)

## ----cyclists1-----------------------------------------------------------
data("CyclingDeaths", package = "vcdExtra")
CyclingDeaths.tab <- table(CyclingDeaths$deaths)
CyclingDeaths.tab

## ----cyclists2, w=8, h=4, out.width=".75\\textwidth", cap="Frequencies of number of cyclist deaths in two-week periods in London, 2005--2012."----
barplot(CyclingDeaths.tab,
        xlab = "Number of deaths", ylab = "Number of fortnights",
        col = "pink", cex.lab = 1.5)

## ----butterfly, w=10, h=4, out.width=".9\\textwidth", cap="Butterfly species in Malaya."----
data("Butterfly", package = "vcd")
barplot(Butterfly, xlab = "Number of individuals", 
        ylab = "Number of species", cex.lab = 1.5)

## ----dbinom12, w=8, h=4, out.width=".75\\textwidth", cap="Binomial distribution for $k=0,\\dots,12$ successes in 12 trials and $p$=1/3."----
k <- 0 : 12
Pk <- dbinom(k, 12, 1/3)
b <- barplot(Pk, names.arg = k,
             xlab = "Number of successes", ylab = "Probability")
lines(x = b, y = Pk, col = "red")

## ----weldon-dbinom-------------------------------------------------------
Weldon_df <- as.data.frame(WeldonDice) # convert to data frame

k <- 0 : 12                       # same as seq(0, 12)
Pk <- dbinom(k, 12, 1/3)          # binomial probabilities
Pk <- c(Pk[1:10], sum(Pk[11:13])) # sum values for 10+
Exp <- round(26306 * Pk, 5)       # expected frequencies
Diff <- Weldon_df$Freq - Exp      # raw residuals
Chisq <- Diff^2 / Exp
data.frame(Weldon_df, Prob = round(Pk, 5), Exp, Diff, Chisq)

## ------------------------------------------------------------------------
p <- c(1/6, 1/3, 1/2, 2/3)
k <- 0 : 12
Prob <- outer(k, p, function(k, p) dbinom(k, 12, p))
str(Prob)

## ----dbinom2-plot2, echo=-1, w=8, h=4, out.width=".75\\textwidth", cap="Binomial distributions for $k=0, \\dots, 12$ successes in $n=12$ trials, and four values of $p$."----
op <- par(mar = c(5,4,1,1) + .1)
col <- palette()[2:5]
matplot(k, Prob,
        type = "o", pch = 15 : 17, col = col, lty = 1,
        xlab = "Number of Successes", ylab = "Probability")
legend("topright", legend = c("1/6","1/3","1/2","2/3"),
       pch = 15 : 17, lty = 1, col = col, title = "Pr(Success)")

## ----soccer-stats1-------------------------------------------------------
data("UKSoccer", package = "vcd")

soccer.df <- as.data.frame(UKSoccer, stringsAsFactors = FALSE)
soccer.df <- within(soccer.df, {
  Home <- as.numeric(Home)       # make numeric
  Away <- as.numeric(Away)       # make numeric
  Total <- Home + Away           # total goals
})
str(soccer.df)

## ----soccer-stats2-------------------------------------------------------
soccer.df <- expand.dft(soccer.df)   # expand to ungrouped form
apply(soccer.df, 2, FUN = function(x) c(mean = mean(x), var = var(x)))

## ----cyclists2-1---------------------------------------------------------
with(CyclingDeaths, c(mean = mean(deaths),
                      var = var(deaths),
                      ratio = mean(deaths) / var(deaths)))

## ----cyclists2-2---------------------------------------------------------
mean.deaths <- mean(CyclingDeaths$deaths)
ppois(5, mean.deaths, lower.tail = FALSE)

## ----dpois1--------------------------------------------------------------
KL <- expand.grid(k = 0 : 20, lambda = c(1, 4, 10))
pois_df <- data.frame(KL, prob = dpois(KL$k, KL$lambda))
pois_df$lambda = factor(pois_df$lambda)
str(pois_df)

## ----dpois-xyplot1, h=4, w=9, out.width="\\textwidth", cap="Poisson distributions for $\\lambda$ = 1, 4, 10, in a multi-panel display."----
library(lattice)
xyplot(prob ~ k | lambda, data = pois_df,
  type = c("h", "p"), pch = 16, lwd = 4, cex = 1.25, layout = c(3, 1),
  xlab = list("Number of events (k)", cex = 1.25),
  ylab = list("Probability", cex = 1.25))

## ----dpois-xyplot2, h=4, w=8, out.width=".8\\textwidth", cap="Poisson distributions for $\\lambda$ = 1, 4, 10, using direct labels."----
mycol <- palette()[2:4]
plt <- xyplot(prob ~ k, data = pois_df, groups = lambda,
  type = "b", pch = 15 : 17, lwd = 2, cex = 1.25, col = mycol,
  xlab = list("Number of events (k)", cex = 1.25),
  ylab = list("Probability",  cex = 1.25),
  ylim = c(0, 0.4))

library(directlabels)
direct.label(plt, list("top.points", cex = 1.5, dl.trans(y = y + 0.1)))

## ----dpois-ggplot1-------------------------------------------------------
library(ggplot2)
gplt <- ggplot(pois_df,
          aes(x = k, y = prob, colour = lambda, shape = lambda)) +
  geom_line(size = 1) + geom_point(size = 3) +
  xlab("Number of events (k)") +
  ylab("Probability")

## ----dpois-ggplot2, h=4, w=8, out.width=".8\\textwidth", cap="Poisson distributions for $\\lambda$ = 1, 4, 10, using \\pkg{ggplot2}."----
gplt + theme(legend.position = c(0.8, 0.8)) +  # manually move legend
       theme(axis.text = element_text(size = 12),
             axis.title = element_text(size = 14, face = "bold"))

## ----dnbin1--------------------------------------------------------------
k <- 2
n <- 2 : 4
p <- 0.2
dnbinom(k, n, p)

(mu <- n * (1 - p) / p)
dnbinom(k, n, mu = mu)

## ----dnbin2--------------------------------------------------------------
XN <- expand.grid(k = 0 : 20, n = c(2, 4, 6), p = c(0.2, 0.3, 0.4))
nbin_df <- data.frame(XN, prob = dnbinom(XN$k, XN$n, XN$p))
nbin_df$n <- factor(nbin_df$n)
nbin_df$p <- factor(nbin_df$p)
str(nbin_df)

## ----dnbin3, h=8, w=8, out.width=".8\\textwidth", cap="Negative binomial distributions for $n = 2, 4, 6$ and $p=0.2, 0.3, 0.4$, using \\func{xyplot}.", fig.pos='!htb'----
xyplot(prob ~ k | n + p, data = nbin_df,
  xlab = list("Number of failures (k)", cex = 1.25),
  ylab = list("Probability",  cex = 1.25),
  type = c("h", "p"), pch = 16, lwd = 2,
  strip = strip.custom(strip.names = TRUE)
  )

## ----dnbin5--------------------------------------------------------------
n <- c(2, 4, 6)
p <- c(0.2, 0.3, 0.4)
NP <- outer(n, p, function(n, p) n * (1 - p) / p)
dimnames(NP) <- list(n = n, p = p)
NP

## ----horse-gof1----------------------------------------------------------
# goodness-of-fit test
tab <- as.data.frame(HorseKicks, stringsAsFactors = FALSE)
colnames(tab) <- c("nDeaths", "Freq")
str(tab)
(lambda <- weighted.mean(as.numeric(tab$nDeaths), w = tab$Freq))

## ----horse-gof2----------------------------------------------------------
phat <- dpois(0 : 4, lambda = lambda)
exp <- sum(tab$Freq) * phat
chisq <- (tab$Freq - exp)^2 / exp

GOF <- data.frame(tab, phat, exp, chisq)
GOF

## ----horse-gof3----------------------------------------------------------
sum(chisq)  # chi-square value
pchisq(sum(chisq), df = nrow(tab) - 2, lower.tail = FALSE)

## ----sax.fit-------------------------------------------------------------
data("Saxony", package = "vcd")
Sax_fit <- goodfit(Saxony, type = "binomial")
unlist(Sax_fit$par) # estimated parameters

## ----sax.fit1------------------------------------------------------------
names(Sax_fit)     # components of "goodfit" objects
Sax_fit            # print method
summary(Sax_fit)   # summary method

## ----dice.fit------------------------------------------------------------
data("WeldonDice", package = "vcd")
dice_fit <- goodfit(WeldonDice, type = "binomial",
                    par = list(size = 12))
unlist(dice_fit$par)

## ----dice.fit1-----------------------------------------------------------
print(dice_fit, digits = 0)
summary(dice_fit)

## ----HF.fit--------------------------------------------------------------
data("HorseKicks", package = "vcd")
HK_fit <- goodfit(HorseKicks, type = "poisson")
HK_fit$par
HK_fit

## ----HF.fit1-------------------------------------------------------------
summary(HK_fit)

## ----Fedfit1-------------------------------------------------------------
data("Federalist", package = "vcd")
Fed_fit0 <- goodfit(Federalist, type = "poisson")
unlist(Fed_fit0$par)
Fed_fit0

## ----Fedfit2-------------------------------------------------------------
summary(Fed_fit0)

## ----Fedfit3-------------------------------------------------------------
Fed_fit1 <- goodfit(Federalist, type = "nbinomial")
unlist(Fed_fit1$par)
summary(Fed_fit1)

## ----Fed0-plots1, h=6, w=6, out.width='.48\\textwidth', cap='Plots for the Federalist Papers data, fitting the Poisson model. Each panel shows the observed frequencies as bars and the fitted frequencies as a smooth curve. Left: raw frequencies; right: plotted on a square-root scale to emphasize smaller frequencies.'----
plot(Fed_fit0, scale = "raw", type = "standing")
plot(Fed_fit0, type = "standing")

## ----Fed0-plots2, h=6, w=6, out.width='.48\\textwidth', cap='Plots for the Federalist Papers data, fitting the Poisson model. Left: hanging rootogram; Right: deviation rootogram. Color reflects the sign and magnitude of the contributions to lack of fit.'----
plot(Fed_fit0, type = "hanging", shade = TRUE)
plot(Fed_fit0, type = "deviation", shade = TRUE)

## ----Fed0-Fed1, h=5, w=6, out.width='.48\\textwidth', cap='Hanging rootograms for the Federalist Papers data, comparing the Poisson and negative binomial models.', fig.pos="!b"----
plot(Fed_fit0, main = "Poisson", shade = TRUE, legend = FALSE)
plot(Fed_fit1, main = "Negative binomial", shade = TRUE, legend = FALSE)

## ----But-fit, h=5, w=6, out.width='.48\\textwidth', cap='Hanging rootograms for the Butterfly data, comparing the Poisson and negative binomial models. The lack of fit for both is readily apparent.'----
data("Butterfly", package = "vcd")
But_fit1 <- goodfit(Butterfly, type = "poisson")
But_fit2 <- goodfit(Butterfly, type = "nbinomial")
plot(But_fit1, main = "Poisson", shade = TRUE, legend = FALSE)
plot(But_fit2, main = "Negative binomial", shade = TRUE, legend = FALSE)

## ----ordplot1, h=6, w=6, out.width='.5\\textwidth', cap='Ord plot for the Butterfly data. The slope and intercept in the plot correctly diagnoses the log-series distribution.'----
ord <- Ord_plot(Butterfly,
                main = "Butterfly species collected in Malaya",
                gp = gpar(cex = 1), pch = 16)
ord

## ----ord-horse-----------------------------------------------------------
data("HorseKicks", package = "vcd")
nk <- as.vector(HorseKicks)
k <- as.numeric(names(HorseKicks))
nk1 <- c(NA, nk[-length(nk)])
y <- k * nk / nk1
weight <- sqrt(pmax(nk, 1) - 1)
(ord_df <- data.frame(k, nk, nk1, y, weight))
coef(lm(y ~ k, weights = weight, data = ord_df))

## ----ordplot2, h=6, w=6, out.width='.5\\textwidth', cap='Ord plot for the HorseKicks data. The plot correctly diagnoses the Poisson distribution.'----
Ord_plot(HorseKicks,
         main = "Death by horse kicks", gp = gpar(cex = 1), pch = 16)

## ----ordplot3, h=6, w=6, out.width='.48\\textwidth', fig.keep="none"-----
Ord_plot(Federalist, main = "Instances of 'may' in Federalist Papers",
         gp = gpar(cex = 1), pch = 16)

## ----ordplot3plot, h=6, w=6, out.width='.48\\textwidth', cap='Ord plots for the Federalist (left) and WomenQueue (right) data sets.', echo=FALSE, fig.pos="!b"----
Ord_plot(Federalist, main = "Instances of 'may' in Federalist Papers",
         gp=gpar(cex=1), pch=16)
Ord_plot(WomenQueue, main = "Women in queues of length 10",
         gp=gpar(cex=1), pch=16)

## ----queues--------------------------------------------------------------
data("WomenQueue", package = "vcd")
WomenQueue

## ----ordplot4, h=6, w=6, out.width='.48\\textwidth', fig.keep="none"-----
Ord_plot(WomenQueue, main = "Women in queues of length 10",
         gp = gpar(cex = 1), pch = 16)

## ----distplot1, w=6, h=6, fig.show='hide'--------------------------------
data("HorseKicks", package = "vcd")
dp <- distplot(HorseKicks, type = "poisson",
  xlab = "Number of deaths", main = "Poissonness plot: HorseKicks data")
print(dp, digits = 4)

## ----distplot2, w=6, h=6, fig.show='hide'--------------------------------
# leveled version, specifying lambda
distplot(HorseKicks, type = "poisson", lambda = 0.61,
  xlab = "Number of deaths", main = "Leveled Poissonness plot")

## ----distplot3, h=6, w=6, out.width=".49\\textwidth", cap='Diagnostic plots for males in Saxony families. Left: \\func{goodfit} plot; right: \\func{distplot} plot. Both plots show heavier tails than in a binomial distribution.'----
plot(goodfit(Saxony, type = "binomial", par = list(size=12)), 
     shade=TRUE, legend=FALSE,
     xlab = "Number of males")
distplot(Saxony, type = "binomial", size = 12,
  xlab = "Number of males")

## ----distplot5, h=6, w=6, out.width=".49\\textwidth", cap='Diagnostic plots for the Federalist Papers data. Left: Poissonness plot; right: negative binomialness plot.'----
distplot(Federalist, type = "poisson", xlab = "Occurrences of 'may'")
distplot(Federalist, type = "nbinomial", xlab = "Occurrences of 'may'")

## ----sax-glm1------------------------------------------------------------
data("Saxony", package = "vcd")
Males <- as.numeric(names(Saxony))
Families <- as.vector(Saxony)
Sax.df <- data.frame(Males, Families)

## ----sax-glm2------------------------------------------------------------
# fit binomial (12, p) as a glm
Sax.bin <- glm(Families ~ Males, offset = lchoose(12, 0:12),
               family = poisson, data = Sax.df)

# brief model summaries
LRstats(Sax.bin)
coef(Sax.bin)

## ----sax-glm3------------------------------------------------------------
# double binomial, (12, p, psi)
Sax.df$YlogitY <-
  Males      * log(ifelse(Males == 0, 1, Males)) +
	(12-Males) * log(ifelse(12-Males == 0, 1, 12-Males))

Sax.dbin <- glm(Families ~ Males + YlogitY, offset = lchoose(12,0:12),
	family = poisson, data = Sax.df)
coef(Sax.dbin)
LRstats(Sax.bin, Sax.dbin)

## ----sax-glm4------------------------------------------------------------
results <- data.frame(Sax.df,
          fit.bin = fitted(Sax.bin), res.bin = rstandard(Sax.bin),
          fit.dbin = fitted(Sax.dbin), res.dbin = rstandard(Sax.dbin))
print(results, digits = 2)

## ----sax-glm5, eval=FALSE, h=6, w=6, out.width=".6\\textwidth", cap='Rootogram for the double binomial model for the Saxony data. This now fits well in the tails of the distribution.'----
## with(results, vcd::rootogram(Families, fit.dbin, Males,
##                         xlab = "Number of males"))

## ----phdpubs0-1----------------------------------------------------------
data("PhdPubs", package = "vcdExtra")
table(PhdPubs$articles)

## ----phdpubs-rootogram, h=5, w=6, out.width='.48\\textwidth', cap='Hanging rootograms for publications by PhD candidates, comparing the Poisson and negative binomial models. The Poisson model clearly does not fit. The the negative binomial is better, but still has significant lack of fit.'----
library(vcd)
plot(goodfit(PhdPubs$articles), xlab = "Number of Articles",
     main = "Poisson")
plot(goodfit(PhdPubs$articles, type = "nbinomial"),
     xlab = "Number of Articles", main = "Negative binomial")

## ----phdpubs0-2----------------------------------------------------------
summary(goodfit(PhdPubs$articles, type = "nbinomial"))

