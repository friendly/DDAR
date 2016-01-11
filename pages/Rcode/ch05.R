## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch05")
.locals$ch05 <- NULL
.pkgs$ch05 <- NULL
library(MASS)

## ----haireye-mos1, h=6, w=6, out.width='.6\\textwidth', cap='Basic mosaic display for hair color and eye color data.  The area of each rectangle is proportional to the observed frequency in that cell, shown as numbers.', echo=FALSE----
library(vcd)
library(vcdExtra)
data("HairEyeColor", package = "datasets")
haireye <- margin.table(HairEyeColor, 1:2)
mosaic(haireye, pop = FALSE)
labeling_cells(text = haireye, gp_text = gpar(fontface = 2), clip = FALSE)(haireye)

## ----haireye-mos2, fig.show='hide', fig.keep='none'----------------------
data("HairEyeColor", package = "datasets")
haireye <- margin.table(HairEyeColor, 1 : 2)
mosaic(haireye, labeling = labeling_values)

## ----haireye-mos3--------------------------------------------------------
(hair <- margin.table(haireye, 1))
prop.table(hair)

## ----haireye-mos5--------------------------------------------------------
expected <- rep(sum(hair) / 4, 4)
names(expected) <- names(hair)
expected

## ------------------------------------------------------------------------
(residuals <- (hair - expected) / sqrt(expected))

## ----haireye-mos7--------------------------------------------------------
round(addmargins(prop.table(haireye, 1), 2), 3)

## ----haireye-mos8, h=6, w=6, out.width='.6\\textwidth', cap='Second step in constructing the mosaic display.  Each rectangle for hair color is subdivided in proportion to the relative frequencies of eye color, and the tiles are shaded in relation to residuals from the model of independence.',echo=FALSE,results="hide"----
mosaic(haireye, shade=TRUE, suppress=0,
        labeling=labeling_residuals, gp_text=gpar(fontface=2))

## ----echo=FALSE,fig.show='hide',fig.keep='none'--------------------------
mosaic(haireye, shade = TRUE, labeling = labeling_residuals)

## ------------------------------------------------------------------------
exp <- independence_table(haireye)
resids <- (haireye - exp) / sqrt(exp)
round(resids, 2)

## ----he-chisq------------------------------------------------------------
(chisq <- sum(resids ^ 2))
(df <- prod(dim(haireye) - 1))
pchisq(chisq, df, lower.tail = FALSE)

## ----he-chisqtest--------------------------------------------------------
chisq.test(haireye)
round(residuals(chisq.test(haireye)), 2)

## ----haireye-mos9, h=6, w=6, out.width='.6\\textwidth', cap='Two-way mosaic for hair color and eye color, reordered. The eye colors were reordered from dark to light, enhancing the interpretation.'----
# re-order eye colors from dark to light
haireye2 <- as.table(haireye[, c("Brown", "Hazel", "Green", "Blue")])
mosaic(haireye2, shade = TRUE)

## ----HE-fill1, h=6, w=6, fig.show='hide'---------------------------------
# color by hair color
fill_colors <- c("brown4", "#acba72", "green", "lightblue")
(fill_colors_mat <- t(matrix(rep(fill_colors, 4), ncol = 4)))
mosaic(haireye2, gp = gpar(fill = fill_colors_mat, col = 0))

## ----shadingMarimekko,fig.show="hide"------------------------------------
mosaic(haireye2, gp = shading_Marimekko(haireye2))

## ----HE-fill2, h=6, w=6, fig.show='hide'---------------------------------
# toeplitz designs
library(colorspace)
toeplitz(1 : 4)
fill_colors <- rainbow_hcl(8)[1 + toeplitz(1 : 4)]
mosaic(haireye2, gp = gpar(fill = fill_colors, col = 0))

## ----HE-fill, h=6, w=6, echo=FALSE, out.width='.49\\textwidth', cap='Mosaic displays for the \\texttt{haireye2} data,  using custom colors to fill the tiles. Left: Marimekko chart, using colors to reflect the eye colors; right: Toeplitz-based colors, reflecting the diagonal strips in a square table.'----
fill_colors <- c("brown4", "#acba72", "green", "lightblue")
fill_colors_mat <- t(matrix(rep(fill_colors, 4), ncol=4))
mosaic(haireye2, gp = gpar(fill = fill_colors_mat, col = 0))

library(colorspace)
#toeplitz(1:4)
fill_colors <- rainbow_hcl(8)[1+toeplitz(1:4)]
mosaic(haireye2, gp = gpar(fill = fill_colors, col = 0))

## ----shadingDiagonal,fig.show="hide"-------------------------------------
mosaic(haireye2, gp = shading_diagonal(haireye2))

## ----HE-fill3, eval=FALSE------------------------------------------------
## mosaic(haireye2, highlighting = "Eye", highlighting_fill = fill_colors)
## mosaic(Eye ~ Hair, data = haireye2, highlighting_fill = fill_colors)

## ----HE-interp, h=6, w=6, out.width='.49\\textwidth', cap='Interpolation options for shading levels in mosaic displays. Left: four shading levels; right: continuous shading.'----
# more shading levels
mosaic(haireye2, shade = TRUE, gp_args = list(interpolate = 1 : 4))

# continuous shading
interp <- function(x) pmin(x / 6, 1)
mosaic(haireye2, shade = TRUE, gp_args = list(interpolate = interp))

## ----HE-shading, h=6, w=6, out.width='.49\\textwidth', cap="Shading functions for mosaic displays. Left: \\code{shading\\_Friendly} using fixed cutoffs and the ``Friendly'' color scheme and an alternative legend style (\\code{legend\\_fixed}); right: \\code{shading\\_max}, using a permutation-based test to determine significance of residuals."----
mosaic(haireye2, gp = shading_Friendly, legend = legend_fixed)
set.seed(1234)
mosaic(haireye2, gp = shading_max)

## ----art-setup-----------------------------------------------------------
art <- xtabs(~ Treatment + Improved, data = Arthritis,
             subset = Sex == "Female")
names(dimnames(art))[2] <- "Improvement"

## ----arth-mosaic, h=6, w=6, out.width='.49\\textwidth', cap="Mosaic plots for the female patients in the \\code{Arthritis} data. Left: Fixed shading levels via \\code{shading\\_Friendly}; right: shading levels determined by significant maximum residuals via \\code{shading\\_max}."----
mosaic(art, gp = shading_Friendly, margin = c(right = 1),
       labeling = labeling_residuals, suppress = 0, digits = 2)
set.seed(1234)
mosaic(art, gp = shading_max, margin = c(right = 1))

## ----arth-residuals------------------------------------------------------
residuals(chisq.test(art))

## ----arth-max------------------------------------------------------------
set.seed(1243)
art_max <- coindep_test(art)
art_max

## ----arth-quantiles------------------------------------------------------
art_max$qdist(c(0.90, 0.99))

## ----soccer-chisq--------------------------------------------------------
data("UKSoccer", package = "vcd")
CMHtest(UKSoccer)

## ----UKsoccer-mosaic, h=6, w=6, out.width='.6\\textwidth', cap='Mosaic display for UK soccer scores, highlighting one cell that stands out for further attention.'----
set.seed(1234)
mosaic(UKSoccer, gp = shading_max, labeling = labeling_residuals,
       digits = 2)

## ----HEC-mos1b, h=6, w=6, out.width='.7\\textwidth', cap='Three-way mosaic for hair color, eye color, and sex.', fig.pos='!htb'----
HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"),]
mosaic(HEC, rot_labels = c(right = -45))

## ----ch5-loglm1, eval=FALSE----------------------------------------------
## loglin(mytable, margin = list(1, 2))

## ----ch5-loglm2, eval=FALSE----------------------------------------------
## loglin(mytable, margin = list(c(1, 2)))

## ----ch5-loglm3, eval=FALSE----------------------------------------------
## loglm(~ A + B, data = mytable)

## ----ch5-loglm4, eval=FALSE----------------------------------------------
## loglm(~ A + B + A : B, data = mytable)
## loglm(~ A * B, data = mytable)

## ------------------------------------------------------------------------
loglm(~ Hair + Eye, data = haireye)

## ------------------------------------------------------------------------
HE_S <- loglm(~ Hair * Eye + Sex, data = HairEyeColor)
HE_S

## ----eval=FALSE----------------------------------------------------------
## residuals(HE_S, type = "pearson")

## ----HEC-mos1, h=6, w=6, out.width='.7\\textwidth', cap='Three-way mosaic for hair color, eye color, and sex. Residuals from the model of joint independence, [HE][S] are shown by shading.', fig.pos='!htb'----
HEC <- HairEyeColor[, c("Brown", "Hazel", "Green", "Blue"),]
mosaic(HEC, expected = ~ Hair * Eye + Sex,
       labeling = labeling_residuals,
       digits = 2, rot_labels = c(right = -45))

## ----HEC-mos2, h=6, w=6, out.width='.49\\textwidth', cap="Mosaic displays for other models fit to the data on hair color, eye color, and sex.  Left: Mutual independence model; right: Conditional independence of hair color and eye color given sex."----
abbrev <- list(abbreviate = c(FALSE, FALSE, 1))
mosaic(HEC, expected = ~ Hair + Eye + Sex, labeling_args = abbrev,
  main = "Model: ~ Hair + Eye + Sex")
mosaic(HEC, expected = ~ Hair * Sex + Eye * Sex, labeling_args = abbrev,
	main="Model: ~ Hair*Sex + Eye*Sex")

## ----HEC-loglm1----------------------------------------------------------
library(MASS)
# three types of independence:
mod1 <- loglm(~ Hair + Eye + Sex, data = HEC)       # mutual
mod2 <- loglm(~ Hair * Sex + Eye * Sex, data = HEC) # conditional
mod3 <- loglm(~ Hair * Eye + Sex, data = HEC)       # joint
LRstats(mod1, mod2, mod3)

## ----HEC-loglm2----------------------------------------------------------
anova(mod1)
anova(mod1, mod2, mod3, test = "chisq")

## ----HEC-seq1, h=6, w=6, echo=FALSE, fig.show='hide'---------------------
mosaic(HEC, expected = ~ Hair + Eye + Sex, legend = FALSE, labeling_args = abbrev, main = "Mutual")

## ----HEC-seq2, h=6, w=6, echo=FALSE, fig.show='hide'---------------------
mosaic(~ Hair + Eye, data = HEC, shade = TRUE, legend = FALSE, main = "Marginal")

## ----HEC-seq3, h=6, w=6, echo=FALSE, fig.show='hide'---------------------
mosaic(HEC, expected = ~ Hair * Eye + Sex, legend = FALSE, labeling_args = abbrev, main = "Joint")

## ----seq-functions-------------------------------------------------------
for(nf in 2 : 5) {
  print(loglin2string(joint(nf, factors = LETTERS[1:5])))
}
for(nf in 2 : 5) {
  print(loglin2string(conditional(nf, factors = LETTERS[1:5]), 
                      sep = ""))
}
for(nf in 2 : 5) {
  print(loglin2formula(conditional(nf, factors = LETTERS[1:5])))
}

## ----seq-functions2------------------------------------------------------
loglin2formula(joint(3, table = HEC))
loglin2string(joint(3, table = HEC))

## ------------------------------------------------------------------------
HEC.mods <- seq_loglm(HEC, type = "joint")
LRstats(HEC.mods)

## ----presex1-------------------------------------------------------------
data("PreSex", package = "vcd")
structable(Gender + PremaritalSex + ExtramaritalSex ~ MaritalStatus, 
           data = PreSex)

## ----presex-reorder------------------------------------------------------
PreSex <- aperm(PreSex, 4 : 1)   # order variables G, P, E, M

## ----presex2, h=6, w=6, out.width='.49\\textwidth', cap='Mosaic displays for the first two marginal tables in the PreSex data. Left: Gender and premarital sex; right: fitting the model of joint independence with extramarital sex, [GP][E].'----
# (Gender Pre)
mosaic(margin.table(PreSex, 1 : 2), shade = TRUE,
                main = "Gender and Premarital Sex")

## (Gender Pre)(Extra)
mosaic(margin.table(PreSex, 1 : 3),
       expected = ~ Gender * PremaritalSex + ExtramaritalSex,
       main = "Gender*Pre + ExtramaritalSex")

## ----presex-odds---------------------------------------------------------
loddsratio(margin.table(PreSex, 1 : 3), stratum = 1, log = FALSE)

## ----presex3, h=6, w=6, out.width='.49\\textwidth', cap='Four-way mosaics for the PreSex data. The left panel fits the model [GPE][M]. The pattern of residuals suggests other associations with marital status. The right panel fits the model [GPE][PEM].'----
## (Gender Pre Extra)(Marital)
mosaic(PreSex,
       expected = ~ Gender * PremaritalSex * ExtramaritalSex
                  + MaritalStatus,
       main = "Gender*Pre*Extra + MaritalStatus")
## (GPE)(PEM)
mosaic(PreSex,
       expected = ~ Gender * PremaritalSex * ExtramaritalSex
                  + MaritalStatus * PremaritalSex * ExtramaritalSex,
       main = "G*P*E + P*E*M")

## ----employ1, size = "footnotesize"--------------------------------------
data("Employment", package = "vcd")
structable(Employment)

## ----employ2-------------------------------------------------------------
loglm(~ EmploymentStatus + EmploymentLength * LayoffCause, 
      data = Employment)

## ----employ-mos1, h=6, w=6, out.width='.6\\textwidth', cap='Mosaic display for the employment status data, fitting the baseline model of joint independence.'----
# baseline model [A][BC]
mosaic(Employment, shade = TRUE,
       expected = ~ EmploymentStatus + EmploymentLength * LayoffCause,
       main = "EmploymentStatus + Length * Cause")

## ----employ3-------------------------------------------------------------
loglm(~ EmploymentStatus * LayoffCause + EmploymentLength * LayoffCause,
      data = Employment)

## ----employ-mos2, h=6, w=6, out.width='.6\\textwidth', cap='Mosaic display for the employment status data, fitting the model of conditional independence, [AC][BC].', scap='Mosaic display for the employment status data, fitting the model of conditional independence'----
mosaic(Employment, shade = TRUE, gp_args = list(interpolate = 1 : 4),
       expected = ~ EmploymentStatus * LayoffCause + 
                    EmploymentLength * LayoffCause,
       main = "EmploymentStatus * Cause + Length * Cause")

## ----employ4-------------------------------------------------------------
mods.list <-
  apply(Employment, "LayoffCause",
        function(x) loglm(~ EmploymentStatus + EmploymentLength, 
                          data = x))
mods.list

## ----employ-mos3, h=6, w=6, out.width='.49\\textwidth', cap='Mosaic displays for the employment status data, with separate panels for cause of layoff.', fig.pos="htb"----
mosaic(Employment[,,"Closure"], shade = TRUE, 
       gp_args = list(interpolate = 1 : 4),
       margin = c(right = 1), main = "Layoff: Closure")
mosaic(Employment[,,"Replaced"], shade = TRUE, 
       gp_args = list(interpolate = 1 : 4),
       margin = c(right = 1), main = "Layoff: Replaced")

## ----punish1-------------------------------------------------------------
data("Punishment", package = "vcd")
str(Punishment, vec.len = 2)

## ----punish2-------------------------------------------------------------
pun <- xtabs(Freq ~ memory + attitude + age + education, 
             data = Punishment)
dimnames(pun) <- list(
  Memory = c("yes", "no"),
  Attitude = c("no", "moderate"),
  Age = c("15-24", "25-39", "40+"),
  Education = c("Elementary", "Secondary", "High"))

## ----punish3-------------------------------------------------------------
(mod.cond <- loglm(~ Memory * Age * Education + 
                     Attitude * Age * Education, data = pun))

## ----punish4-------------------------------------------------------------
set.seed(1071)
coindep_test(pun, margin = c("Age", "Education"),
             indepfun = function(x) sum(x ^ 2), aggfun = sum)

## ----punish-cond1-bis, h=8, w=8, out.width='.95\\textwidth', cap='Conditional mosaic plot of the Punishment data for the model of conditional independence of attitude and memory, given age and education. Shading of tiles is based on the sum of squares statistic.', echo=FALSE, fig.pos='hbt'----
set.seed(1071)
pun_cotab <- cotab_coindep(pun, condvars = 3 : 4, type = "mosaic",
  varnames = FALSE, margins = c(2, 1, 1, 2),
  test = "sumchisq", interpolate = 1 : 2)
cotabplot(~ Memory + Attitude | Age + Education,
          data = pun, panel = pun_cotab)

## ------------------------------------------------------------------------
mods.list <- apply(pun, c("Age", "Education"),
        function(x) loglm(~ Memory + Attitude, data = x)$pearson)

## ----punish-cond1, h=8, w=8, out.width='.95\\textwidth', cap='Conditional mosaic plot of the Punishment data for the model of conditional independence of attitude and memory, given age and education. Shading of tiles is based on the sum of squares statistic.', eval=FALSE----
## set.seed(1071)
## pun_cotab <- cotab_coindep(pun, condvars = 3 : 4, type = "mosaic",
##   varnames = FALSE, margins = c(2, 1, 1, 2),
##   test = "sumchisq", interpolate = 1 : 2)
## cotabplot(~ Memory + Attitude | Age + Education,
##           data = pun, panel = pun_cotab)

## ----punish-cond2, h=8, w=8, out.width='.8\\textwidth', cap='Conditional mosaic plot of the Punishment data for the model of conditional independence of attitude and memory, given age and education. This plot explicitly shows the total frequencies in the cells of age and education by the areas of the main blocks for these variables.', fig.pos='!htb'----
mosaic(~ Memory + Attitude | Age + Education, data = pun,
       shade = TRUE, gp_args = list(interpolate = 1 : 4))

## ----bartlett-pairs, h=8, w=8, out.width='.8\\textwidth', cap='Mosaic pairs plot for the Bartlett data. Each panel shows the bivariate marginal relation between the row and column variables.', fig.pos='!htb'----
pairs(Bartlett, gp = shading_Friendly2)

## ----marital-pairs,h=8, w=8, out.width='.8\\textwidth', cap='Mosaic pairs plot for the PreSex data. Each panel shows the bivariate marginal relation between the row and column variables.', fig.pos='!htb'----
data("PreSex", package = "vcd")
pairs(PreSex, gp = shading_Friendly2, space = 0.25,
      gp_args = list(interpolate = 1 : 4), 
      diag_panel_args = list(offset_varnames = -0.5))

## ----berk-pairs1, h=8, w=8, out.width='.8\\textwidth', cap='Mosaic matrix of the UCBAdmissions data showing bivariate marginal relations.', fig.pos='htb'----
largs <- list(labeling = labeling_border(varnames = FALSE,
              labels = c(T, T, F, T), alternate_labels = FALSE))
dargs <- list(gp_varnames = gpar(fontsize = 20), offset_varnames = -1,
              labeling = labeling_border(alternate_labels = FALSE))
pairs(UCBAdmissions, shade = TRUE, space = 0.25,
      diag_panel_args = dargs,
      upper_panel_args = largs, lower_panel_args = largs)

## ----berk-pairs2, h=8, w=8, out.width='.8\\textwidth', cap='Generalized mosaic matrix of the UCBAdmissions data. The above-diagonal plots fit models of joint independence; below-diagonal plots fit models of mutual independence.', fig.pos='!htb'----
pairs(UCBAdmissions, space = 0.2,
      lower_panel = pairs_mosaic(type = "joint"),
      upper_panel = pairs_mosaic(type = "total"))

## ----berk-pairs3, eval=FALSE---------------------------------------------
## pairs(UCBAdmissions, type = "conditional", space = 0.2)

## ----arth-gpairs, h=8, w=8, out.width='.9\\textwidth', cap='Generalized pairs plot of the Arthritis data. Combinations of categorical and quantitative variables can be rendered in various ways.', fig.pos='!htb'----
library(gpairs)
data("Arthritis", package = "vcd")
gpairs(Arthritis[,c(5, 2, 3, 4)],
       diag.pars = list(fontsize = 20),
       mosaic.pars = list(gp = shading_Friendly,
                          gp_args = list(interpolate = 1 : 4)))

## ----mos3d1, eval=FALSE--------------------------------------------------
## mosaic3d(Bartlett)

## ----struc1--------------------------------------------------------------
struc <- array(c(6, 10, 312, 44,
                 37, 31, 192, 76),
 dim = c(2, 2, 2),
 dimnames = list(Age = c("Young", "Old"),
                 Sex = c("F", "M"),
                 Disease = c("No", "Yes"))
 )
struc <- as.table(struc)
structable(struc)

## ----struc-mos1, h=6, w=6, out.width='.6\\textwidth', cap='Mosaic display for the data on age, sex, and disease. Observed frequencies are shown in the plot, and residuals reflect departure from the model of mutual independence.', fig.pos="!b"----
mosaic(struc, shade = TRUE)

## ----struc-mos2, h=6, w=6, out.width='.5\\textwidth', cap='Mosaic display for the data on age, sex, and disease, using expected frequencies under mutual independence.'----
mosaic(struc, type = "expected")

## ----struc2--------------------------------------------------------------
mutual <- loglm(~ Age + Sex + Disease, data = struc, fitted = TRUE)
fit <- as.table(fitted(mutual))
structable(fit)

## ----struc-mos3, h=8, w=8, out.width='.8\\textwidth', cap='Mosaic matrix for fitted values under mutual independence.  In all panels the joint frequencies conform to the one-way margins.', fig.pos='!htb'----
pairs(fit, gp = shading_Friendly2, type = "total")

## ----code-mos3d1, eval=FALSE---------------------------------------------
## mosaic3d(fit)

## ----struc3--------------------------------------------------------------
joint <- loglm(~ Age * Sex + Disease, data = struc, fitted = TRUE)
fit <- as.table(fitted(joint))
structable(fit)

## ----struc-mos4, h=8, w=8, out.width='.7\\textwidth', cap='Mosaic matrix for fitted values under joint independence for the model [Age Sex][Disease].', scap='Mosaic matrix for fitted values under joint independence.', fig.pos='!htb'----
pairs(fit, gp = shading_Friendly2)

## ----sec-related, child='ch05/related.Rnw'-------------------------------

## ----berkeley-doubledecker, w=10, h=4, out.width='\\textwidth', cap='Doubledecker plot for the UCBAdmissions data.', fig.pos='htb'----
doubledecker(Admit ~ Dept + Gender, data = UCBAdmissions[2:1, , ])

## ----titanic-doubledecker, w=12, h=4, out.width='\\textwidth', cap='Doubledecker plot for the Titanic data.'----
doubledecker(Survived ~ Class + Age + Sex, Titanic)

## ----pun1, R.options=list(digits=3)--------------------------------------
data("Punishment", package = "vcd")
pun_lor <- loddsratio(Freq ~ memory + attitude | age + education, 
                      data = Punishment)

## ----pun2, R.options=list(digits=3)--------------------------------------
pun_lor_df <- as.data.frame(pun_lor)

## ----pun-lor-plot, h=4, w=5, out.width='.7\\textwidth', cap='Log odds ratio for the association between attitude and memory of corporal punishment, stratified by age and education. Error bars show $\\pm 1$ standard error.', scap='Log odds ratio for the association between attitude and memory of corporal punishment, stratified by age and education', fig.pos="H"----
plot(pun_lor)

## ----pun-anova0, echo=FALSE----------------------------------------------
pun_lor_df <- transform(pun_lor_df, 
    age = as.numeric(age), 
    education = as.numeric(education))

## ----pun-anova-----------------------------------------------------------
pun_mod <- lm(LOR ~ age * education, data = pun_lor_df, 
              weights = 1 / ASE^2)
anova(pun_mod)

## ----titanic-lor1--------------------------------------------------------
Titanic2 <- Titanic[, , 2:1, 2:1]
Titanic2["Crew", , "Child", ] <- NA
titanic_lor1 <- loddsratio(~ Survived + Age | Class + Sex, 
                           data = Titanic2)
titanic_lor1

## ----titanic-lor2--------------------------------------------------------
titanic_lor2 <- loddsratio(~ Survived + Sex | Class + Age, 
                           data = Titanic2)
titanic_lor2

## ----titanic-lor-plot, echo=FALSE, h=6, w=6, out.width='0.49\\textwidth', cap='Log odds ratio plots for the Titanic data. Left: Odds ratios for survival and age, by sex and class. Right: for survival and sex, by age and class. Error bars show $\\pm 1$ standard error.', scap='Log odds ratio plots for the Titanic data.'----
plot(titanic_lor1)
plot(titanic_lor2)


