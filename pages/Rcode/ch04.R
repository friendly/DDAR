## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch04")
.locals$ch04 <- NULL
.pkgs$ch04 <- NULL
library(vcd)
library(vcdExtra)

## ----results="hide",fig.keep="none"--------------------------------------
hec <- margin.table(HairEyeColor, 2:1)
barplot(hec, beside = TRUE, legend = TRUE)

## ----bartile, h=6, w=6, out.width='.49\\textwidth', cap="Two basic displays for the Hair-color Eye-color data. Left: grouped barchart; right: tile plot.", echo=FALSE----
hec <- margin.table(HairEyeColor, 2:1)
barplot(hec, beside = TRUE, legend = TRUE, args.legend = list(x = "top"))
tile(hec)

## ----results="hide",fig.keep="none"--------------------------------------
tile(hec)

## ----spineplot,w=6,h=6,out.width="0.7\\textwidth",cap="Spineplot of the Mental data."----
data("Mental", package = "vcdExtra")
mental <- xtabs(Freq ~ ses + mental, data = Mental)
spineplot(mental)

## ----berk-table----------------------------------------------------------
Berkeley <- margin.table(UCBAdmissions, 2:1)
library(gmodels)
CrossTable(Berkeley, prop.chisq = FALSE, prop.c = FALSE, 
           format = "SPSS")

## ----odds-logoods--------------------------------------------------------
p <- c(0.05, .1, .25, .50, .75, .9, .95)
odds <- p / (1 - p)
logodds <- log(odds)
data.frame(p, odds, logodds)

## ------------------------------------------------------------------------
data("UCBAdmissions")
UCB <- margin.table(UCBAdmissions, 1:2)
(LOR <- loddsratio(UCB))
(OR <- loddsratio(UCB, log = FALSE))

## ------------------------------------------------------------------------
summary(LOR)

## ------------------------------------------------------------------------
confint(OR)
confint(LOR)

## ------------------------------------------------------------------------
fisher.test(UCB)

## ----arth-assocstats1----------------------------------------------------
data("Arthritis", package = "vcd")
Art <- xtabs(~ Treatment + Improved, data = Arthritis)
Art
round(100 * prop.table(Art, margin = 1), 2)

## ----arth-assocstats2----------------------------------------------------
assocstats(Art)

## ------------------------------------------------------------------------
data("Mental", package = "vcdExtra")
mental <- xtabs(Freq ~ ses + mental, data = Mental)
assocstats(mental)    # standard chisq tests
CMHtest(mental)       # CMH tests

## ----chmdemo-prep, results='hide', echo=FALSE----------------------------
# general association
cmhdemo1 <- read.table(header=TRUE, sep="", text="
     b1  b2   b3  b4  b5
a1    0  15   25  15   0
a2    5  20    5  20   5
a3   20   5    5   5  20
")
cmhdemo1 <- as.matrix(cmhdemo1)

# linear association
cmhdemo2 <- read.table(header=TRUE, sep="", text="
     b1  b2   b3  b4  b5
a1    2   5    8   8   8
a2    2   8    8   8   5
a3    5   8    8   8   2
a4    8   8    8   5   2
")

cmhdemo2 <- as.matrix(cmhdemo2)

## ----cmhdemo-test1-------------------------------------------------------
CMHtest(cmhdemo1)

## ----cmhdemo, h=6, w=6, out.width='.49\\textwidth', cap='Sieve diagrams for two patterns of association: Left: general association; right: linear association.',echo=FALSE, fig.pos="tp"----
sieve(cmhdemo1, shade=TRUE, main="General association",
  gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))
sieve(cmhdemo2, shade=TRUE, main="Linear association",
  gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))

## ----cmhdemo-test2-------------------------------------------------------
CMHtest(cmhdemo2)

## ----arth-strat1---------------------------------------------------------
Art2 <- xtabs(~ Treatment + Improved + Sex, data = Arthritis)
Art2

## ----arth-strat2---------------------------------------------------------
assocstats(Art2)

## ----arth-strat3---------------------------------------------------------
CMHtest(Art2)
apply(Art2, 3, sum)

## ----berk-woolf----------------------------------------------------------
woolf_test(UCBAdmissions)

## ----art-homogeneity-----------------------------------------------------
woolf_test(Art2)

## ----art-homogeneity2----------------------------------------------------
library(MASS)
loglm(~ (Treatment + Improved + Sex)^2, data = Art2)

## ----berk-fourfold1, h=6, w=6, out.width='.49\\textwidth', cap='Fourfold displays for the Berkeley admission data. Left: unstandardized; right: equating the proportions of males and females.'----
fourfold(Berkeley, std = "ind.max")   # unstandardized
fourfold(Berkeley, margin = 1)        # equating gender

## ----berk-fourfold3, h=6, w=6, out.width='.6\\textwidth', cap='Fourfold display for Berkeley admission data with margins for gender and admission equated. The area of each quadrant shows the standardized frequency in each cell.'----
fourfold(Berkeley)  # standardize both margins

## ------------------------------------------------------------------------
summary(loddsratio(Berkeley))
exp(.6103 + c(-1, 1) * qnorm(.975) * 0.06398)
confint(loddsratio(Berkeley, log = FALSE))

## ----berk-fourfold4, h=6, w=9, out.width='.95\\textwidth', cap='Fourfold displays for Berkeley admissions data, stratified by department. The more intense shading for Dept. A indicates a significant association.', out.extra='trim=80 50 80 50'----
# fourfold display
UCB <- aperm(UCBAdmissions, c(2, 1, 3))
fourfold(UCB, mfrow = c(2, 3))

## ----coalminer0----------------------------------------------------------
data("CoalMiners", package = "vcd")
CM <- CoalMiners[, , 2 : 9]
structable(. ~ Age, data = CM)

## ----coalminer1, h=5, w=10, out.width='\\textwidth', cap='Fourfold display for CoalMiners data, both margins equated.', out.extra='trim=120 80 120 80'----
fourfold(CM, mfcol = c(2, 4))

## ----coalminer2----------------------------------------------------------
loddsratio(CM)
loddsratio(CM, log = FALSE)

## ----coalminer3, h=6, w=6, out.width='.6\\textwidth', cap='Log odds plot for the CoalMiners data.  The smooth curve shows a quadratic fit to age.'----
lor_CM <- loddsratio(CM)
plot(lor_CM, bars=FALSE, baseline=FALSE, whiskers=.2)

lor_CM_df <- as.data.frame(lor_CM)
age <- seq(25, 60, by = 5) + 2
lmod <- lm(LOR ~ poly(age, 2), weights = 1 / ASE^2, data = lor_CM_df)
grid.lines(seq_along(age), fitted(lmod), 
           gp = gpar(col = "red", lwd = 2), default.units = "native")

## ----coalminer4----------------------------------------------------------
summary(lmod)

## ----HE-expected---------------------------------------------------------
haireye <- margin.table(HairEyeColor, 1:2)
expected = independence_table(haireye)
round(expected, 1)

## ----HE-sieve-simple, eval=FALSE, echo=FALSE-----------------------------
## sieve(haireye, shade=TRUE, sievetype="expected",
##       main="Expected frequencies")
## sieve(haireye, shade=TRUE,
##       main="Observed frequencies")

## ----HE-sieve, h=6, w=6, out.width='.49\\textwidth', cap="Sieve diagrams for the \\data{HairEyeColor} data. Left: expected frequencies shown in cells as numbers and the number of boxes; right: observed frequencies shown in cells.", echo=FALSE, fig.pos="tb"----
sieve(haireye, sievetype = "expected", shade = TRUE,
      main="Expected frequencies",
      labeling = labeling_values, value_type = "expected",
      gp_text = gpar(fontface = 2), gp = shading_sieve(interpolate = 0, line_col="darkgrey",eps=Inf,lty="dashed"))

sieve(haireye, shade = TRUE, main="Observed frequencies",
      labeling = labeling_values, value_type = "observed",
      gp_text = gpar(fontface = 2))

## ----VA.tab--------------------------------------------------------------
# re-assign names/dimnames
data("VisualAcuity", package = "vcd")
VA <- xtabs(Freq ~ right + left + gender, data = VisualAcuity)
dimnames(VA)[1:2] <- list(c("high", 2, 3, "low"))
names(dimnames(VA))[1:2] <- paste(c("Right", "Left"), "eye grade")
structable(aperm(VA))

## ----VA-sieve2, h=6, w=6, out.width='.7\\textwidth', cap='Vision classification for 7,477 women in Royal Ordnance factories. The high frequencies in the diagonal cells indicate the main association, but a subtler pattern also appears in the symmetric off-diagonal cells.', fig.pos='H'----
sieve(VA[, , "female"], shade = TRUE)

## ----sieve3, eval=FALSE--------------------------------------------------
## sieve(Freq ~ right + left, data = VisualAcuity, shade = TRUE)

## ----VA-sieve3, h=6, w=6, out.width='.7\\textwidth', cap='Sieve diagram for the three-way table of VisualAcuity, conditioned on gender.'----
sieve(Freq ~ right + left | gender, data = VisualAcuity, 
      shade = TRUE, set_varnames = c(right = "Right eye grade", 
                                     left = "Left eye grade"))

## ----VA-cotabsieve3, h=6, w=12, out.width='\\textwidth', cap='Conditional Sieve diagram for the three-way table of VisualAcuity, conditioned on gender.'----
cotabplot(VA, cond = "gender", panel = cotab_sieve, shade = TRUE)

## ----berkeley-sieve, h=6, w=6, out.width='.49\\textwidth', cap='Sieve diagrams for the three-way table of the Berkeley admissions data. Left: Admit by Dept, conditioned on Gender; right: Dept re-ordered as the first splitting variable.'----
# conditioned on gender
sieve(UCBAdmissions, shade = TRUE, condvar = 'Gender')
# three-way table, Department first, with cell labels
sieve(~ Dept + Admit + Gender, data = UCBAdmissions, 
      shade = TRUE, labeling = labeling_values, 
      gp_text = gpar(fontface = 2), abbreviate_labs = c(Gender = TRUE))

## ----berkeley-cotabsieve, h=6, w=12, out.width='\\textwidth', cap='Conditional Sieve diagram for the three-way table of the Berkeley data, conditioned on gender.'----
cotabplot(UCBAdmissions, cond = "Gender", panel = cotab_sieve, 
          shade = TRUE)

## ----berkeley-cotabsieve2, h=8, w=12, out.width='\\textwidth', cap='Conditional Sieve diagram for the three-way table of the Berkeley data, conditioned on department.'----
cotabplot(UCBAdmissions, cond = "Dept", panel = cotab_sieve, 
          shade = TRUE, labeling = labeling_values, 
          gp_text = gpar(fontface = "bold"))

## ----berkeley-sieve2, h=6, w=6, out.width='.6\\textwidth', cap='Sieve diagram for the Berkeley admissions data, fitting the model of joint independence, Admit * Gender + Dept.'----
UCB2 <- aperm(UCBAdmissions, c(3, 2, 1))
sieve(UCB2, shade = TRUE, expected = ~ Admit * Gender + Dept,
      split_vertical = c(FALSE, TRUE, TRUE))

## ----eval=FALSE----------------------------------------------------------
## assoc(~ Hair + Eye, data = HairEyeColor, shade = TRUE)
## assoc(HairEyeColor, shade = TRUE)

## ----HE-assoc, h=6, w=6, out.width='0.5\\textwidth', cap='Association plot for the hair-color eye-color data. Left: marginal table, collapsed over gender; right: full table.',echo=FALSE----
assoc(~ Hair + Eye, data = HairEyeColor, shade = TRUE, gp_axis = gpar(lty = 5))
assoc(HairEyeColor, shade = TRUE, gp_axis = gpar(lty = 5))

## ----sexisfun------------------------------------------------------------
data("SexualFun", package = "vcd")
SexualFun

## ----ms1-----------------------------------------------------------------
MSPatients[, , "Winnipeg"]
MSPatients[, , "New Orleans"]
apply(MSPatients, 3, sum)      # show sample sizes

## ----sexfun-kappa--------------------------------------------------------
Kappa(SexualFun)
confint(Kappa(SexualFun))

## ----sexfun-agree, echo=2:3, h=6, w=6, out.width='.48\\textwidth', cap="Agreement charts for husbands' and wives' sexual fun. Left: unweighted chart, showing only exact agreement; right: weighted chart, using weight $w_1 = 8/9$ for a one-step disagreement.", fig.pos="hbt"----
op <- par(mar=c(4,3,4,1)+.1)
agreementplot(SexualFun, main = "Unweighted", weights = 1)
agreementplot(SexualFun, main = "Weighted")
par(op)

## ----sexfun-agree1, fig.keep='none'--------------------------------------
B <- agreementplot(SexualFun)
unlist(B)[1 : 2]

## ----mammograms1, echo=2:3, h=6, w=6, out.width='.6\\textwidth', cap='Agreement plot for the Mammograms data.'----
op <- par(mar = c(4, 3, 4, 1) + .1)
data("Mammograms", package = "vcdExtra")
B <- agreementplot(Mammograms, main = "Mammogram ratings")
par(op)

## ----mammograms2---------------------------------------------------------
unlist(B)[1 : 2]

## ----MS-agree, h=7, w=12, out.width='\\textwidth', cap='Weighted agreement charts for both patient samples in the MSPatients data. Departure of the middle rectangles from the diagonal indicates lack of marginal homogeneity.'----
cotabplot(MSPatients, cond = "Patients", panel = cotab_agreementplot,
          text_gp = gpar(fontsize = 18), xlab_rot=20)

## ----MS-agree-stats, fig.keep='none'-------------------------------------
agr1 <- agreementplot(MSPatients[, , "Winnipeg"])
agr2 <- agreementplot(MSPatients[, , "New Orleans"])
rbind(Winnipeg = unlist(agr1), NewOrleans = unlist(agr2))[, 1 : 2]

## ----tripdemo2, h=6, w=6, out.width='.6\\textwidth', cap='A trilinear plot showing three points, for variables A, B, C.', out.extra='trim=20 20 20 20,clip',echo=FALSE, fig.pos="!b"----
library(ggtern)
DATA <- data.frame(
  A = c(40, 20, 10),
  B = c(30, 60, 10),
  C = c(30, 20, 80),
  id = c("1", "2", "3"))
ggtern(data = DATA,
       mapping = aes(x=C, y=A, z=B, colour = id)) +
    geom_point(size=4) +
    geom_text(vjust=-.5, size=8, aes(label=id), show_guide=FALSE) +
    theme_rgbw() +
    theme(plot.margin=unit(c(0,0,0,0),"mm"))

## ----eval=FALSE----------------------------------------------------------
## library(ggtern)
## DATA <- data.frame(
##   A = c(40, 20, 10),
##   B = c(30, 60, 10),
##   C = c(30, 20, 80),
##   id = c("1", "2", "3"))
## 
## aesthetic_mapping <- aes(x = C, y = A, z = B, colour = id)
## ggtern(data = DATA, mapping = aesthetic_mapping) +
##     geom_point(size = 4) +
##     theme_rgbw()

## ----eval=FALSE----------------------------------------------------------
## data("Lifeboats", package = "vcd")
## # label boats with more than 10% men
## Lifeboats$id <- ifelse(Lifeboats$men / Lifeboats$total > .1,
##                        as.character(Lifeboats$boat), "")
## 
## AES <- aes(x = women, y = men, z = crew, colour = side, shape = side,
##            label = id)
## ggtern(data = Lifeboats, mapping = AES) +
##     geom_text() +
##     geom_point(size=2) +
##     geom_smooth_tern(method = "lm", alpha = 0.2)

## ----lifeboats1, h=6, w=6, out.width='.7\\textwidth', cap='Lifeboats on the \\emph{Titanic}, showing the composition of each boat.  Boats with more than 10\\% male passengers are labeled.', scap='Lifeboats on the Titanic', out.extra='clip,trim=0 20 0 20',echo=FALSE----
data("Lifeboats", package="vcd")
# label boats with more than 10% men
Lifeboats$id <- ifelse(Lifeboats$men/Lifeboats$total > .1,
                       as.character(Lifeboats$boat), "")
ggtern(data = Lifeboats, 
       mapping = aes(x = women, y = men, z = crew, colour=side, shape=side, label=id)) +
    theme_rgbw() +
    geom_point(size=2) +
    labs(title = "Lifeboats on the Titanic") +
    labs(T="Women and children") +
    geom_smooth_tern(method="lm", size=1.5, alpha=.2, aes(fill=side)) +
    geom_text(vjust=1, color="black") +
    theme(legend.position=c(.85, .85), axis.tern.vshift=unit(5,"mm"))

## ----eval=FALSE----------------------------------------------------------
## AES <- aes(x = launch, y = total, colour = side, label = boat)
## ggplot(data = Lifeboats, mapping = AES) +
##      geom_text() +
##      geom_smooth(method = "lm", aes(fill = side), size = 1.5) +
##      geom_smooth(method = "loess", aes(fill = side), se = FALSE,
##                  size = 1.2)

## ----lifeboats2, h=5, w=9, out.width='.8\\textwidth', cap='Number of people loaded on lifeboats on the Titanic vs. time of launch, by side of boat. The plot annotations show the linear regression and loess smooth.',echo=FALSE----
ggplot(data = Lifeboats,
       aes(x=launch, y=total, colour=side,  label=boat)) +
     geom_smooth(method="lm", aes(fill=side), size=1.5) +
     geom_smooth(method="loess", aes(fill=side), se=FALSE, size=1.2) +
     geom_point() + ylim(c(0,100)) +
     geom_text(vjust=-.5, color="black") +
     labs(y="Total loaded", x="Launch time")

