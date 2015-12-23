## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch10")
library(vcdExtra)
.locals$ch10 <- NULL
.pkgs$ch10 <- NULL

## ----ordinal,  child='ch10/ordinal.Rnw'----------------------------------

## ----lodds, R.options=list(digits=3)-------------------------------------
library(vcd)
data("Mental", package = "vcdExtra")
(mental.tab <- xtabs(Freq ~ mental + ses, data=Mental))
(LMT <- loddsratio(mental.tab))

## ----mental-lorplot-plot, eval=FALSE-------------------------------------
## library(corrplot)
## corrplot(as.matrix(LMT), method = "square", is.corr = FALSE,
##          tl.col = "black", tl.srt = 0, tl.offset = 1)

## ----lodds1--------------------------------------------------------------
mean(LMT$coefficients)

## ----mental-glm1---------------------------------------------------------
indep <- glm(Freq ~ mental + ses, data = Mental, family = poisson)
LRstats(indep)

## ----mental-indep, h=6, w=7, out.width='.7\\textwidth', cap='Mosaic display of the independence model for the mental health data.'----
long.labels <- list(set_varnames = c(mental="Mental Health Status",
                                     ses="Parent SES"))
mosaic(indep,
       gp=shading_Friendly,
       residuals_type="rstandard",
       labeling_args = long.labels,
       labeling=labeling_residuals, suppress=1,
       main="Mental health data: Independence")

## ----mental-glm2---------------------------------------------------------
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)

## ----mental-glm3---------------------------------------------------------
linlin <- update(indep, . ~ . + Rscore:Cscore)
roweff <- update(indep, . ~ . + mental:Cscore)
coleff <- update(indep, . ~ . + Rscore:ses)
rowcol <- update(indep, . ~ . + Rscore:ses + mental:Cscore)

## ----mental-glm4, R.options=list(digits=6)-------------------------------
LRstats(indep, linlin, roweff, coleff, rowcol)

## ----mental-glm5---------------------------------------------------------
anova(indep, linlin, roweff, test = "Chisq")
anova(indep, linlin, coleff, test = "Chisq")

## ----mental-glm6---------------------------------------------------------
# interpret linlin association parameter
coef(linlin)[["Rscore:Cscore"]]
exp(coef(linlin)[["Rscore:Cscore"]])

## ----mental-lodds-roweff-------------------------------------------------
roweff.fit <- matrix(fitted(roweff), 4, 6, 
                     dimnames=dimnames(mental.tab))
round(as.matrix(loddsratio(roweff.fit)), 3)

## ----mental-lodds-coleff-------------------------------------------------
coleff.fit <- matrix(fitted(coleff), 4, 6, 
                     dimnames = dimnames(mental.tab))
round(as.matrix(loddsratio(coleff.fit)), 3)

## ----mental-lodds-plots, echo=FALSE, h=5, w=5, out.width='.33\\textwidth', cap='Log odds ratio plots for the R (left), C (middle), and R+C (right) models fit to the mental health data.'----
plot(t(loddsratio(roweff.fit)), confidence = FALSE, 
     legend_pos="bottomright", ylim = c(-.2, .3),
     main = "log odds ratios for ses and mental, R model")
plot(t(loddsratio(coleff.fit)), confidence = FALSE, 
     legend_pos = "bottomright", ylim = c(-.2, .3),
     main="log odds ratios for ses and mental, C model")
rowcol.fit <- matrix(fitted(rowcol), 4, 6, dimnames = dimnames(mental.tab))
plot(t(loddsratio(rowcol.fit)), confidence = FALSE, 
     legend_pos = "bottomright", ylim = c(-.2, .3),
     main = "log odds ratios for ses and mental, R+C model")

## ----mental-gnm1---------------------------------------------------------
library(gnm)
contrasts(Mental$mental) <- contr.treatment
contrasts(Mental$ses) <- contr.treatment
indep <- gnm(Freq ~ mental + ses, data = Mental, family = poisson)
RC1 <- update(indep, . ~ . + Mult(mental, ses), verbose = FALSE)
RC2 <- update(indep, . ~ . + instances(Mult(mental, ses), 2), 
              verbose = FALSE)

## ----mental-gnm2, R.options=list(digits=6)-------------------------------
LRstats(indep, linlin, roweff, coleff, RC1, RC2)

## ----mental-gnm3---------------------------------------------------------
anova(linlin, RC1, RC2, test = "Chisq")

## ----mental-gnm4---------------------------------------------------------
rowProbs <- with(Mental, tapply(Freq, mental, sum) / sum(Freq))
colProbs <- with(Mental, tapply(Freq, ses, sum) / sum(Freq))
mu <- getContrasts(RC1, pickCoef(RC1, "[.]mental"),
                   ref = rowProbs, scaleWeights = rowProbs)
nu <- getContrasts(RC1, pickCoef(RC1, "[.]ses"),
                   ref = colProbs, scaleWeights = colProbs)

## ----mental-gnm5---------------------------------------------------------
(alpha <- mu$qvframe)
(beta  <- nu$qvframe)

## ----mental-gnm6, R.options=list(digits=3)-------------------------------
scores <- rbind(alpha, beta)
scores <- cbind(scores,
                factor = c(rep("mental", 4), rep("ses", 6)) )
rownames(scores) <- c(levels(Mental$mental), levels(Mental$ses))
scores$lower <- scores[,1] - scores[,2]
scores$upper <- scores[,1] + scores[,2]
scores

## ----mental-RC1-plot, h=6, w=8, echo=2, fig.show='hide'------------------
op <- par(mar=c(5, 4, 1, 1) + .1)
with(scores, {
  dotchart(Estimate, groups = factor, labels = rownames(scores),
           cex = 1.2, pch = 16, xlab = "RC1 Score",
           xlim = c(min(lower), max(upper)))
  arrows(lower, c(8 + (1 : 4), 1 : 6), upper, c(8 + (1 : 4), 1 : 6),
         col = "red", angle = 90, length = .05, code = 3, lwd = 2)
  })
par(op)

## ----mental-gnm7---------------------------------------------------------
alpha <- coef(RC2)[pickCoef(RC2, "[.]mental")]
alpha <- matrix(alpha, ncol=2)
rownames(alpha) <- levels(Mental$mental)
colnames(alpha) <- c("Dim1", "Dim2")
alpha

beta <- coef(RC2)[pickCoef(RC2, "[.]ses")]
beta <- matrix(beta, ncol=2)
rownames(beta) <- levels(Mental$ses)
colnames(beta) <- c("Dim1", "Dim2")
beta

## ----mental-logmult1, eval=FALSE-----------------------------------------
## library(logmult)
## rc1 <- rc(mental.tab, verbose = FALSE, weighting = "marginal",
##           se = "jackknife")
## rc2 <- rc(mental.tab, verbose = FALSE, weighting = "marginal", nd = 2,
##           se = "jackknife")

## ----mental-logmult2, eval=FALSE, fig.show='hide'------------------------
## coords  <- plot(rc2, conf.ellipses = 0.68, cex = 1.5,
##                 rev.axes = c(TRUE, FALSE))

## ----mental-logmult3, eval=FALSE-----------------------------------------
## scores <- rbind(coords$row, coords$col)
## lines(scores[1 : 4,], col = "blue", lwd = 2)
## lines(scores[-(1 : 4),], col = "red", lwd = 2)


## ----square, child='ch10/square.Rnw'-------------------------------------

## ----vision-glm1---------------------------------------------------------
data("VisualAcuity", package="vcd")
women <- subset(VisualAcuity, gender=="female", select=-gender)

## ----vision-glm2---------------------------------------------------------
#library(vcdExtra)
indep <- glm(Freq ~ right + left,  data = women, family = poisson)
quasi <- update(indep, . ~ . + Diag(right, left))

symm <- glm(Freq ~ Symm(right, left), data = women, family = poisson)
qsymm <- update(symm, . ~ right + left + .)

## ----vision-glm4---------------------------------------------------------
LRstats(indep, quasi, symm, qsymm)

## ----vision-mosaics, h=6, w=6, out.width='.49\\textwidth', cap='Mosaic displays comparing the models of quasi-independence and quasi-symmetry for visual acuity in women.'----
labs <- c("High", "2", "3", "Low")
largs <- list(set_varnames = c(right = "Right eye grade",
                               left = "Left eye grade"),
              set_labels=list(right = labs, left = labs))
mosaic(quasi, ~ right + left, residuals_type = "rstandard",
       gp = shading_Friendly,
       labeling_args = largs,
       main = "Quasi-Independence (women)")
mosaic(qsymm, ~ right + left, residuals_type = "rstandard",
       gp = shading_Friendly,
       labeling_args = largs,
       main = "Quasi-Symmetry (women)")

## ----vision-glm5---------------------------------------------------------
anova(symm, qsymm, test = "Chisq")

## ----hauser1-------------------------------------------------------------
data("Hauser79", package = "vcdExtra")
structable(~ Father + Son, data = Hauser79)

## ----hauser-lor, R.options=list(digits=3)--------------------------------
hauser.tab <- xtabs(Freq ~ Father + Son, data = Hauser79)
(lor.hauser <- loddsratio(hauser.tab))

## ----hauser-lor-plot, h=6, w=7, out.width='.6\\textwidth', cap='Plot of observed local log odds ratios in the Hauser79 data. The dotted horizontal line at zero shows local independence; the solid black horizontal line shows the mean.'----
plot(lor.hauser, confidence = FALSE, legend_pos = "topleft", 
     xlab = "Father's status comparisons")
m <- mean(lor.hauser$coefficients)        # mean LOR
grid.lines(x = unit(c(0, 1), "npc"),
           y = unit(c(m, m), "native"))

## ----hauser2-------------------------------------------------------------
hauser.indep <- gnm(Freq ~ Father + Son, data = Hauser79, 
                    family = poisson)
hauser.quasi <-  update(hauser.indep, ~ . + Diag(Father, Son))
LRstats(hauser.indep, hauser.quasi)

## ----hauser-mosaic1, h=6, w=6, out.width='.49\\textwidth', cap='Mosaic displays for the Hauser79 data. Left: independence model; right:quasi-independence model.'----
mosaic(hauser.indep, ~ Father + Son, main = "Independence model",
       gp = shading_Friendly)
mosaic(hauser.quasi, ~ Father + Son, main = "Quasi-independence model",
       gp = shading_Friendly)

## ----hauser-qsymm--------------------------------------------------------
hauser.qsymm <-  update(hauser.indep,
                        ~ . + Diag(Father, Son) + Symm(Father, Son))
LRstats(hauser.qsymm)

## ----hauser-mosaic2, h=6, w=6, out.width='.6\\textwidth', cap='Mosaic display  for the model of quasi-symmetry fit to the Hauser79 data.'----
mosaic(hauser.qsymm, ~ Father + Son, main = "Quasi-symmetry model",
       gp = shading_Friendly, residuals_type = "rstandard")

## ----hauser-topo1--------------------------------------------------------
levels <- matrix(c(
  2,  4,  5,  5,  5,
  3,  4,  5,  5,  5,
  5,  5,  5,  5,  5,
  5,  5,  5,  4,  4,
  5,  5,  5,  4,  1
  ), 
  5, 5, byrow = TRUE)

## ----hauser-topo2--------------------------------------------------------
hauser.topo <- update(hauser.indep, 
                      ~ . + Topo(Father, Son, spec = levels))
LRstats(hauser.topo)

## ----hauser-topo3--------------------------------------------------------
as.vector((coef(hauser.topo)[pickCoef(hauser.topo, "Topo")]))

## ----hauser-summary------------------------------------------------------
LRstats(hauser.indep, hauser.quasi, hauser.qsymm, hauser.topo)

## ----hauser2-ord1--------------------------------------------------------
Fscore <- as.numeric(Hauser79$Father)   # numeric scores
Sscore <- as.numeric(Hauser79$Son)      # numeric scores

# uniform association
hauser.UA <- update(hauser.indep, ~ . + Fscore * Sscore)
# row effects model
hauser.roweff <- update(hauser.indep, ~ . + Father * Sscore)
# RC model
hauser.RC <- update(hauser.indep, 
                    ~ . + Mult(Father, Son), verbose = FALSE)

## ----hauser2-ord2--------------------------------------------------------
LRstats(hauser.indep, hauser.UA, hauser.roweff, hauser.RC)

## ----hauser-ord3---------------------------------------------------------
hauser.UAdiag <- update(hauser.UA, ~ . + Diag(Father, Son))
anova(hauser.UA, hauser.UAdiag, test = "Chisq")

## ----hauser2-ord3--------------------------------------------------------
coef(hauser.UAdiag)[["Fscore:Sscore"]]

## ----hauser2-CR1---------------------------------------------------------
hauser.CR <- update(hauser.indep, ~ . + Crossings(Father, Son))
hauser.CRdiag <- update(hauser.CR, ~ . + Diag(Father, Son))
LRstats(hauser.CR, hauser.CRdiag)

## ----hauser2-CR2---------------------------------------------------------
nu <- coef(hauser.CRdiag)[pickCoef(hauser.CRdiag, "Crossings")]
names(nu) <- gsub("Crossings(Father, Son)C", "nu", names(nu), 
                  fixed = TRUE)
nu

## ----hauser-mosaic3, h=6, w=6, out.width='.6\\textwidth', cap='Mosaic display  for the quasi-crossings model fit to the Hauser79 data.'----
mosaic(hauser.CRdiag, ~ Father + Son,
       gp = shading_Friendly, residuals_type = "rstandard",
       main = "Crossings() + Diag()")

## ----hauser-sumry1-------------------------------------------------------
modlist <- glmlist(hauser.indep, hauser.roweff, hauser.UA, 
                   hauser.UAdiag, hauser.quasi, hauser.qsymm, 
                   hauser.topo, hauser.RC, hauser.CR, hauser.CRdiag)
LRstats(modlist, sortby = "BIC")

## ----hauser-sumry-plot, h=6, w=7, out.width='.7\\textwidth', cap='Model comparison plot for the models fit to the Hauser79 data.'----
sumry <- LRstats(modlist)
mods <- substring(rownames(sumry), 8)
with(sumry, {
  plot(Df, BIC, cex = 1.3, pch = 19,
       xlab = "Degrees of freedom", ylab = "BIC (log scale)",
       log = "y", cex.lab = 1.2)
  pos <- ifelse(mods == "UAdiag", 1, 3)
  text(Df, BIC + 55, mods, pos = pos, col = "red", xpd = TRUE, cex = 1.2)
  })


## ----ord3way, child='ch10/ord3way.Rnw'-----------------------------------

## ----vision2-kway0, eval=FALSE-------------------------------------------
## Freq ~ 1
## Freq ~ right + left + gender
## Freq ~ (right + left + gender)^2
## Freq ~ (right + left + gender)^3

## ----vision2-kway--------------------------------------------------------
vis.kway <- Kway(Freq ~ right + left + gender, data = VisualAcuity)
LRstats(vis.kway)

## ----vision2-glm1--------------------------------------------------------
vis.indep <- glm(Freq ~ right + left + gender,  data = VisualAcuity,
                 family = poisson)
vis.quasi <- update(vis.indep, . ~ . + Diag(right, left))
vis.qsymm <- update(vis.indep, . ~ . + Diag(right, left) 
                                     + Symm(right, left))

LRstats(vis.indep, vis.quasi, vis.qsymm)

## ----vision2-qsymm, h=6, w=6, out.width='.7\\textwidth', cap='Mosaic display for the model of homogeneous quasi-symmetry fit to the VisualAcuity data.', fig.pos='!htb'----
mosaic(vis.qsymm, ~ gender + right + left, condvars = "gender",
       residuals_type = "rstandard", gp = shading_Friendly,
       labeling_args = largs, rep = FALSE,
       main = "Homogeneous quasi-symmetry")

## ----vision2-glm2--------------------------------------------------------
vis.hetdiag <- update(vis.indep, . ~ . + gender * Diag(right, left) +
                      Symm(right, left))
vis.hetqsymm <- update(vis.indep, . ~ . + gender * Diag(right, left) +
                       gender * Symm(right, left))
LRstats(vis.qsymm, vis.hetdiag, vis.hetqsymm)

## ----vision2-hetqsymm, h=6, w=6, out.width='.7\\textwidth', cap='Mosaic display for the model of heterogeneous quasi-symmetry fit to the VisualAcuity data.', fig.pos='!htb'----
mosaic(vis.hetqsymm, ~ gender + right + left, condvars="gender",
       residuals_type = "rstandard", gp = shading_Friendly,
       labeling_args = largs, rep = FALSE,
       main="Heterogeneous quasi-symmetry")


## ----multiv, child='ch10/multiv.Rnw'-------------------------------------



## ----coalminers-ex, child='ch10/coalminers.Rnw'--------------------------

## ----cm1-----------------------------------------------------------------
data("CoalMiners", package = "vcd")
coalminers <- data.frame(t(matrix(aperm(CoalMiners, c(2, 1, 3)),
                                  4, 9)))
colnames(coalminers) <- c("BW", "Bw", "bW", "bw")
coalminers$age <- c(22, 27, 32, 37, 42, 47, 52, 57, 62)
coalminers

## ----blogits2------------------------------------------------------------
logitsCM <- blogits(coalminers[, 1 : 4], add = 0.5)
colnames(logitsCM)[1:2] <- c("logitB", "logitW")
logitsCM

## ----cm-blogits, h=6, w=7, out.width='.8\\textwidth', cap='Empirical logits and log odds ratio for breathlessness and wheeze in the CoalMiners data. The lines show separate linear regressions for each function. The right vertical axis shows equivalent probabilities for the logits.', fig.pos='!htb'----
col <- c("blue", "red", "black")
pch <- c(15, 17, 16)
age <- coalminers$age

op <- par(mar = c(4, 4, 1, 4)+.2)
matplot(age, logitsCM, type = "p",
  col = col, pch = pch, cex = 1.2, cex.lab = 1.25,
  xlab = "Age", ylab = "Log Odds or Odds Ratio")
abline(lm(logitsCM[,1] ~ age), col = col[1], lwd = 2)
abline(lm(logitsCM[,2] ~ age), col = col[2], lwd = 2)
abline(lm(logitsCM[,3] ~ age), col = col[3], lwd = 2)

# right probability axis
probs <- c(.01, .05, .10, .25, .5)
axis(4, at = qlogis(probs), labels = probs)
mtext("Probability", side = 4, cex = 1.2, at = -2, line = 2.5)
# curve labels
text(age[2], logitsCM[2, 1] + .5, "Breathlessness", 
     col = col[1], pos = NULL, cex = 1.2)
text(age[2], logitsCM[2, 2] + .5, "Wheeze", 
     col = col[2], pos = NULL, cex = 1.2)
text(age[2], logitsCM[2, 3] - .5, "log OR\n(B|W)/(B|w)", 
     col = col[3], pos = 1, cex = 1.2)
par(op)

## ----cm-glm1-------------------------------------------------------------
CM <- as.data.frame(CoalMiners)
colnames(CM)[1:2] <- c("B", "W")
head(CM)

## ----cm-glm2-------------------------------------------------------------
cm.glm0 <- glm(Freq ~ B + W + Age, data = CM, family = poisson)
cm.glm1 <- glm(Freq ~ B * W + Age, data = CM, family = poisson)
LRstats(cm.glm0, cm.glm1)

## ----cm-mosaic1, h=6, w=7, out.width='.7\\textwidth', cap='Mosaic display for the baseline model, [BW][Age], fit to the CoalMiners data.', scap='Mosaic display for the baseline model, BW Age, fit to the CoalMiners data.'----
vnames <- list(set_varnames = c(B = "Breathlessness", W = "Wheeze"))
lnames <- list(B=c("B", "b"), W = c("W", "w"))
mosaic(cm.glm1, ~ Age + B + W,
       labeling_args = vnames, set_labels = lnames)

## ----cm-glm3-------------------------------------------------------------
cm.glm2 <- glm(Freq ~ B * W + (B + W) * Age, data = CM, family = poisson)
LRstats(cm.glm1, cm.glm2)

## ----cm-glm4-------------------------------------------------------------
library(car)
Anova(cm.glm2)

## ----cm-glm5-------------------------------------------------------------
CM$age <- rep(seq(22, 62, 5), each = 4)

## ----cm-glm6-------------------------------------------------------------
CM$ageOR <- (CM$B == "B") * (CM$W == "W") * CM$age
cm.glm3 <- update(cm.glm2, . ~ . + ageOR)
LRstats(cm.glm0, cm.glm1, cm.glm2, cm.glm3)

## ----cm.glm7-------------------------------------------------------------
anova(cm.glm2, cm.glm3, test = "Chisq")

## ----cm-vglm0------------------------------------------------------------
coalminers <- transform(coalminers, agec = (age - 42) / 5)
coalminers$Age <- dimnames(CoalMiners)[[3]]
coalminers

## ----cm-vglm1------------------------------------------------------------
library(VGAM)
#                      00  01  10  11
cm.vglm1 <- vglm(cbind(bw, bW, Bw, BW) ~ agec,
                 binom2.or(zero = NULL), data = coalminers)
cm.vglm1

## ------------------------------------------------------------------------
(G2 <- deviance(cm.vglm1))
# test residual deviance
1-pchisq(deviance(cm.vglm1), cm.vglm1@df.residual)

## ----cm-vglm1-coef-------------------------------------------------------
coef(cm.vglm1, matrix = TRUE)
exp(coef(cm.vglm1, matrix = TRUE))

## ----cm-vglm1-plot1------------------------------------------------------
age <- coalminers$age
P <- fitted(cm.vglm1)
colnames(P) <- c("bw", "bW", "Bw", "BW")
head(P)
Y <- depvar(cm.vglm1)

## ----cm-vglm1-plot2, eval=FALSE, fig.show='hide', size='footnotesize'----
## col <- c("red", "blue", "red", "blue")
## pch <- c(1, 2, 16, 17)
## 
## op <- par(mar = c(5, 4, 1, 1) + .1)
## matplot(age, P, type = "l",
##   col = col,
##   lwd = 2, cex = 1.2, cex.lab = 1.2,
##   xlab = "Age", ylab = "Probability",
##   xlim = c(20,65))
## matpoints(age, Y, pch = pch, cex = 1.2, col = col)
## # legend
## text(64, P[9,]+ c(0,.01, -.01, 0), labels = colnames(P), col = col, cex = 1.2)
## text(20, P[1,]+ c(0,.01, -.01, .01), labels = colnames(P), col = col, cex = 1.2)
## par(op)

## ----cm-lP, eval=FALSE---------------------------------------------------
## lP <- qlogis(P)
## lY <- qlogis(Y)

## ----cm-blogitsP, eval=FALSE---------------------------------------------
## # blogits, but for B and W
## logitsP <- blogits(P[, 4 : 1])
## logitsY <- blogits(Y[, 4 : 1])

## ----cm-vglm2------------------------------------------------------------
cm.vglm2 <- vglm(cbind(bw, bW, Bw, BW) ~ poly(agec, 2),
                 binom2.or(zero = NULL), data = coalminers)

## ----cm-vglm2-LR---------------------------------------------------------
(LR <- deviance(cm.vglm1) - deviance(cm.vglm2))
1 - pchisq(LR, cm.vglm1@df.residual - cm.vglm2@df.residual)


## ----tox-ex, child='ch10/toxaemia.Rnw'-----------------------------------

## ----tox1, size='footnotesize'-------------------------------------------
data("Toxaemia", package = "vcdExtra")
str(Toxaemia)
tox.tab <- xtabs(Freq ~ class + smoke + hyper + urea, Toxaemia)
ftable(tox.tab, row.vars = 1)

## ----tox-mosaic1, h=6, w=6, out.width='.5\\textwidth', cap='Mosaic displays for Toxaemia data: Predictor and response associations.'----
mosaic(~ smoke + class, data = tox.tab, shade = TRUE,
       main = "Predictors", legend = FALSE)
mosaic(~ hyper + urea, data = tox.tab, shade = TRUE,
       main = "Responses", legend = FALSE)

## ----tox-mosaic2, h=4, w=12, out.width='1.1\\textwidth', cap='Toxaemia data: Response association conditioned on smoking level.'----
cotabplot(~ hyper + urea | smoke, tox.tab, shade = TRUE,
          legend = FALSE, layout = c(1, 3))

## ----tox-mosaic3, h=4, w=20, out.width='1.1\\textwidth', cap='Toxaemia data: Response association conditioned on social class.'----
cotabplot(~ hyper + urea | class, tox.tab, shade = TRUE,
          legend = FALSE, layout = c(1, 5))

## ----tox-fourfold, eval=FALSE--------------------------------------------
## fourfold(aperm(tox.tab), fontsize = 16)

## ----tox-margins---------------------------------------------------------
margin.table(tox.tab, 2 : 1)

## ----tox-LOR0------------------------------------------------------------
(LOR <- loddsratio(urea ~ hyper | smoke + class, data = tox.tab))

## ----tox-LOR, h=6, w=9, out.width='.75\\textwidth', cap='Log odds ratios for protein urea given hypertension, by social class and level of maternal smoking.'----
plot(t(LOR), confidence = FALSE, legend_pos = "bottomright", 
     xlab = "Social class of mother")

## ----tox-logits----------------------------------------------------------
tox.hyper <- glm(hyper == "High" ~ class * smoke, weights = Freq,
                 data = Toxaemia, family = binomial)
tox.urea <- glm(urea == "High" ~ class * smoke, weights = Freq,
                data = Toxaemia, family = binomial)

## ----tox-effplots, h=6, w=6, out.width='.5\\textwidth', cap='Effect plots for hypertension and urea, by social class of mother and smoking.'----
library(effects)

plot(allEffects(tox.hyper),
  ylab = "Probability (hypertension)",
  xlab = "Social class of mother",
  main = "Hypertension: class*smoke effect plot",
  colors = c("blue", "black", "red"),
  lwd=3,  multiline = TRUE,
  key.args = list(x = 0.05, y = 0.2, cex = 1.2, columns = 1)
  )

plot(allEffects(tox.urea),
  ylab = "Probability (Urea)",
  xlab = "Social class of mother",
  main = "Urea: class*smoke effect plot",
  colors = c("blue", "black", "red"),
  lwd=3,  multiline = TRUE,
  key.args = list(x = 0.65, y = 0.2, cex = 1.2, columns = 1)
  )

## ----tox-prep------------------------------------------------------------
tox.tab <- xtabs(Freq~class + smoke + hyper + urea, Toxaemia)
toxaemia <- t(matrix(aperm(tox.tab), 4, 15))
colnames(toxaemia) <- c("hu", "hU", "Hu", "HU")
rowlabs <- expand.grid(smoke = c("0", "1-19", "20+"), 
                       class = factor(1:5))
toxaemia <- cbind(toxaemia, rowlabs)
head(toxaemia)

## ----tox-vglm1-----------------------------------------------------------
tox.vglm1 <- vglm(cbind(hu, hU, Hu, Hu) ~ class + smoke,
                  binom2.or(zero = 3), data = toxaemia)
coef(tox.vglm1, matrix=TRUE)

## ----tox-glms1-----------------------------------------------------------
# null model
tox.glm0 <- glm(Freq ~ class*smoke + hyper + urea,
                data = Toxaemia, family = poisson)
# baseline model: no association between predictors and responses
tox.glm1 <- glm(Freq ~ class*smoke + hyper*urea,
                data = Toxaemia, family = poisson)

## ----tox-glms2, size='footnotesize'--------------------------------------
tox.glm2 <- update(tox.glm1, . ~ . + smoke*hyper + class*urea)

tox.glm3 <- glm(Freq ~ (class + smoke + hyper + urea)^2,
                data=Toxaemia, family=poisson)

tox.glm4 <- glm(Freq ~ class*smoke*hyper + hyper*urea + class*urea,
                data=Toxaemia, family=poisson)

tox.glm5 <- update(tox.glm4, . ~ . + smoke*urea)

tox.glm6 <- update(tox.glm4, . ~ . + class*smoke*urea)

tox.glm7 <- update(tox.glm6, . ~ . + smoke*hyper*urea)

tox.glm8 <- glm(Freq ~ (class + smoke + hyper + urea)^3,
                data = Toxaemia, family = poisson)

tox.glm9 <- glm(Freq ~ (class + smoke + hyper + urea)^4,
                data = Toxaemia, family = poisson)

## ----tox-glms3, size='footnotesize'--------------------------------------
library(lmtest)
lmtest::lrtest(tox.glm1, tox.glm2, tox.glm3, tox.glm4, tox.glm5)

## ----tox-logits1---------------------------------------------------------
# reshape to 15 x 4 table of frequencies
tox.tab <- xtabs(Freq ~ class + smoke + hyper + urea, Toxaemia)
toxaemia <- t(matrix(aperm(tox.tab), 4, 15))
colnames(toxaemia) <- c("hu", "hU", "Hu", "HU")
rowlabs <- expand.grid(smoke = c("0", "1-19", "20+"), 
                       class = factor(1:5))
toxaemia <- cbind(toxaemia, rowlabs)

## ----tox-logits2---------------------------------------------------------
# observed logits and log odds ratios
logitsTox <- blogits(toxaemia[,4:1], add=0.5)
colnames(logitsTox)[1:2] <- c("logitH", "logitU")
logitsTox <- cbind(logitsTox, rowlabs)
head(logitsTox)

## ----tox-logits3---------------------------------------------------------
# fitted frequencies, as a 15 x 4 table
Fit <- t(matrix(predict(tox.glm2, type = "response"), 4, 15))
colnames(Fit) <- c("HU", "Hu", "hU", "hu")
Fit <- cbind(Fit, rowlabs)
logitsFit <- blogits(Fit[, 1 : 4], add=0.5)
colnames(logitsFit)[1 : 2] <- c("logitH", "logitU")
logitsFit <- cbind(logitsFit, rowlabs)

## ----tox-logits4---------------------------------------------------------
matrix(logitsFit$logOR, 3, 5,
       dimnames = list(smoke = c("0", "1-19", "20+"), class = 1 : 5))

## ----tox-glmplot1, eval=FALSE--------------------------------------------
## ggplot(logitsFit, aes(x = as.numeric(class), y = logitH,
##                       color = smoke)) +
##   theme_bw() +
##   geom_line(size = 1.2) +
##   scale_color_manual(values = c("blue", "black", "red")) +
##   ylab("log odds (Hypertension)") +
##   xlab("Social class of mother") +
##   ggtitle("Hypertension") +
##   theme(axis.title = element_text(size = 16)) +
##   geom_point(data = logitsTox,
##              aes(x = as.numeric(class), y = logitH, color = smoke),
##              size = 3) +
##   theme(legend.position = c(0.85, .6))


