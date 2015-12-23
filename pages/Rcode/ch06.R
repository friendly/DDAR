## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch06")
.locals$ch06 <- NULL
.pkgs$ch06 <- NULL

## ----ca-haireye1, R.options=list(digits=4)-------------------------------
haireye <- margin.table(HairEyeColor, 1 : 2)
library(ca)
(haireye.ca <- ca(haireye))

## ----ca-haireye2, output.lines=9-----------------------------------------
summary(haireye.ca)

## ----ca-haireye3---------------------------------------------------------
# standard coordinates Phi (Eqn 6.4) and Gamma (Eqn 6.5)
(Phi <- haireye.ca$rowcoord)
(Gamma <- haireye.ca$colcoord)

# demonstrate orthogonality of std coordinates
Dr <- diag(haireye.ca$rowmass)
zapsmall(t(Phi) %*% Dr %*% Phi)
Dc <- diag(haireye.ca$colmass)
zapsmall(t(Gamma) %*% Dc %*% Gamma)

## ----ca-haireye-plot, echo=2, h=5.53, w=7, out.width='.8\\textwidth', cap='Correspondence analysis solution for the hair color and eye color data.'----
op <- par(cex=1.4, mar=c(5,4,1,2)+.1)
res <- plot(haireye.ca)
par(op)

## ----ca-haireye4---------------------------------------------------------
res

## ----ca-mental1----------------------------------------------------------
data("Mental", package="vcdExtra")
mental.tab <- xtabs(Freq ~ ses + mental, data = Mental)

## ----ca-mental2, output.lines=9------------------------------------------
mental.ca <- ca(mental.tab)
summary(mental.ca)

## ----ca-mental-plot, echo=2:4, h=4, w=6, out.width='.9\\textwidth', cap='Correspondence analysis solution for the Mental health data.'----
op <- par(cex=1.3, mar=c(5,4,1,1)+.1)
res <- plot(mental.ca,  ylim = c(-.2, .2))
lines(res$rows, col = "blue", lty = 3)
lines(res$cols, col = "red", lty = 4)
par(op)

## ----ca-victims1, output.lines=13----------------------------------------
data("RepVict", package = "vcd")
victim.ca <- ca(RepVict)
summary(victim.ca)

## ----ca-victims2---------------------------------------------------------
chisq.test(RepVict)
(chisq <- sum(RepVict) * sum(victim.ca$sv^2))

## ----ca-victims-plot, echo=2:4, h=6, w=6, out.width='.65\\textwidth', cap='2D CA solution for the repeat victimization data. Lines connect the category points for first and second occurrence to highlight these relations.', fig.pos='!htb'----
op <- par(cex=1.3, mar=c(4,4,1,1)+.1)
res <- plot(victim.ca, labels = c(2, 0))
segments(res$rows[,1], res$rows[,2], res$cols[,1], res$cols[,2])
legend("topleft", legend = c("First", "Second"), title = "Occurrence",
       col = c("blue", "red"), pch = 16 : 17, bg = "gray90")
par(op)

## ----RVsym, fig.show='hide'----------------------------------------------
RVsym <- (RepVict + t(RepVict)) / 2
RVsym.ca <- ca(RVsym)
res <- plot(RVsym.ca)
all.equal(res$rows, res$cols)

## ----TV2-1---------------------------------------------------------------
data("TV", package = "vcdExtra")
TV2 <- margin.table(TV, c(1, 3))
TV2

## ----TV2-ca-results, output.lines=5--------------------------------------
TV.ca <- ca(TV2)
TV.ca

## ----TV2-ca, eval=FALSE--------------------------------------------------
## res <- plot(TV.ca)
## segments(0, 0, res$cols[,1], res$cols[,2], col = "red", lwd = 2)

## ----TV2-mosaic, eval=FALSE----------------------------------------------
## days.order <- order(TV.ca$rowcoord[,1])
## mosaic(t(TV2[days.order,]), shade = TRUE, legend = FALSE,
##        labeling = labeling_residuals, suppress=0)

## ----stacking-demo-------------------------------------------------------
set.seed(1234)
dim <- c(3, 2, 2, 2)
tab <- array(rpois(prod(dim), 15), dim = dim)
dimnames(tab) <- list(Pet = c("dog", "cat", "bird"), 
                      Age = c("young", "old"), 
                      Color = c("black", "white"), 
                      Sex = c("male", "female"))

## ----stacking-demo1------------------------------------------------------
ftable(Pet + Age ~ Color + Sex, tab)

## ----stacking-demo2, R.options=list(width=96)----------------------------
(pet.mat <- as.matrix(ftable(Pet + Age ~ Color + Sex, tab), sep = '.'))

## ----stacking-demo3, results='hide'--------------------------------------
tab.df <- as.data.frame(as.table(tab))
tab.df <- within(tab.df, 
  {Pet.Age = interaction(Pet, Age)
  Color.Sex = interaction(Color, Sex)
  })               
xtabs(Freq ~ Color.Sex + Pet.Age, data = tab.df)

## ----ca-suicide1---------------------------------------------------------
data("Suicide", package = "vcd")
# interactive coding of sex and age.group
Suicide <- within(Suicide, {
  age_sex <- paste(age.group, toupper(substr(sex, 1, 1)))
  })

## ----ca-suicide2---------------------------------------------------------
suicide.tab <- xtabs(Freq ~ age_sex + method2, data = Suicide)
suicide.tab

## ----ca-suicide3, output.lines=13----------------------------------------
suicide.ca <- ca(suicide.tab)
summary(suicide.ca)

## ----ca-suicide-plot, echo=2, h=6, w=6, out.width='.7\\textwidth', cap='2D CA solution for the stacked [AgeSex][Method] table of the suicide data.', scap='2D CA solution for the stacked AgeSex, Method table of the suicide data.'----
op <- par(cex=1.3, mar=c(4,4,1,1)+.1)
plot(suicide.ca)
par(op)

## ----ca-suicide4---------------------------------------------------------
suicide.tab3 <- xtabs(Freq ~ sex + age.group + method2, data = Suicide)

## ----ca-suicide5---------------------------------------------------------
# methods, ordered as in the table
suicide.ca$colnames
# order of methods on CA scores for Dim 1
suicide.ca$colnames[order(suicide.ca$colcoord[,1])]
# reorder methods by CA scores on Dim 1
suicide.tab3 <- suicide.tab3[, , order(suicide.ca$colcoord[,1])]
# delete "other"
suicide.tab3 <- suicide.tab3[,, -5]
ftable(suicide.tab3)

## ----ca-suicide-mosaic, h=6, w=6, out.width='.7\\textwidth', cap='Mosaic display showing deviations from the model [AgeSex][Method] for the suicide data.', scap='Mosaic display showing deviations from the model AgeSex Method for the suicide data', fig.pos='!htb'----
library(vcdExtra)
mosaic(suicide.tab3, shade = TRUE, legend = FALSE,
       expected = ~ age.group * sex + method2,
       labeling_args = list(abbreviate_labs = c(FALSE, FALSE, 5)),
                            rot_labels = c(0, 0, 0, 90))

## ----ca-suicide6---------------------------------------------------------
# two way, ignoring sex
suicide.tab2 <- xtabs(Freq ~ age.group + method2, data = Suicide)
suicide.tab2
suicide.ca2 <- ca(suicide.tab2)

## ----ca-suicide7---------------------------------------------------------
# relation of sex and method
suicide.sup <- xtabs(Freq ~ sex + method2, data = Suicide)
suicide.tab2s <- rbind(suicide.tab2, suicide.sup)

## ----ca-suicide8, output.lines=11----------------------------------------
suicide.ca2s <- ca(suicide.tab2s, suprow = 6 : 7)
summary(suicide.ca2s)

## ----ca-suicide-sup, echo=2:3, h=3, w=6, out.width='.65\\textwidth', cap='2D CA solution for the [Age] [Method] marginal table. Category points for Sex are shown as supplementary points.', scap='2D CA solution for the Age Method marginal table.', fig.pos="H"----
op <- par(cex=1.3, mar=c(4,4,1,1)+.1)
res <- plot(suicide.ca2s, pch = c(16, 15, 17, 24))
lines(res$rows[6 : 7,])
par(op)

## ----mca-indicator1------------------------------------------------------
haireye.df <- cbind(
    as.data.frame(haireye),
    model.matrix(Freq ~ Hair + Eye, data=haireye,
        contrasts.arg=list(Hair=diag(4), Eye=diag(4)))[,-1]
    )
haireye.df

## ----mca-indicator2------------------------------------------------------
Z <- expand.dft(haireye.df)[,-(1:2)]
vnames <- c(levels(haireye.df$Hair), levels(haireye.df$Eye))
colnames(Z) <- vnames
dim(Z)

## ----mca-indicator3------------------------------------------------------
(N <- t(as.matrix(Z[,1:4])) %*% as.matrix(Z[,5:8]))

## ----mca-haireye0, fig.keep='none'---------------------------------------
Z.ca <- ca(Z)
res <- plot(Z.ca, what = c("none", "all"))

## ----mca-haireye1, echo=FALSE, h=6, w=8, out.width='.8\\textwidth', cap='Correspondence analysis of the indicator matrix Z for the hair color--eye color data. The category points are joined separately by lines for the hair color and eye color categories.', fig.pos="t"----
# customized plot
res <- plot(Z.ca, what=c("none", "all"), labels = 0, pch = ".", xpd = TRUE)

# extract factor names and levels
coords <- data.frame(res$cols)
coords$factor <- rep(c("Hair", "Eye"), each = 4)
coords$levels <- rownames(res$cols)
coords
# sort by Dim 1
coords <- coords[ order(coords[,"factor"], coords[,"Dim1"]), ]

cols <- c("blue", "red")
nlev <- c(4,4)
text(coords[,1:2], coords$levels, col=rep(cols, nlev), pos=2, cex=1.2)
points(coords[,1:2], pch=rep(16:17, nlev), col=rep(cols, nlev), cex=1.2)

lines(Dim2 ~ Dim1, data=coords, subset=factor=="Eye",  lty=1, lwd=2, col=cols[1])
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Hair", lty=1, lwd=2, col=cols[2])

## ----Burt1---------------------------------------------------------------
Burt <- t(as.matrix(Z)) %*% as.matrix(Z)
rownames(Burt) <- colnames(Burt) <- vnames
Burt

## ----Burt2, fig.keep='none'----------------------------------------------
Burt.ca <- ca(Burt)
plot(Burt.ca)

## ----presex-mca2, output.lines=10----------------------------------------
data("PreSex", package = "vcd")
PreSex <- aperm(PreSex, 4:1)   # order variables G, P, E, M
presex.mca <- mjca(PreSex, lambda = "Burt")
summary(presex.mca)

## ----presex-mca3, fig.keep='none', results='hide'------------------------
plot(presex.mca)

## ----presex-mca-plot, h=6, w=6, out.width='.7\\textwidth', cap='MCA plot of the Burt matrix for the PreSex data. The category points are joined separately by lines for the factor variables.', fig.pos='!htb', out.extra='trim=0 10 20 60', fig.pos="H"----
# plot, but don't use point labels or points
res <- plot(presex.mca, labels = 0, pch = ".", cex.lab = 1.2)

# extract factor names and levels
coords <- data.frame(res$cols, presex.mca$factors)
nlev <- presex.mca$levels.n
fact <- unique(as.character(coords$factor))

cols <- c("blue", "red", "brown", "black")
points(coords[,1:2], pch=rep(16:19, nlev), col=rep(cols, nlev), cex=1.2)
text(coords[,1:2], label=coords$level, col=rep(cols, nlev), pos=3, 
     cex=1.2, xpd=TRUE)
lwd <- c(2, 2, 2, 4)
for(i in seq_along(fact)) {
  lines(Dim2 ~ Dim1, data = coords, subset = factor==fact[i], 
        lwd = lwd[i], col = cols[i])
}

legend("bottomright", 
       legend = c("Gender", "PreSex", "ExtraSex", "Marital"),
       title = "Factor", title.col = "black",
       col = cols, text.col = cols, pch = 16:19,
       bg = "gray95", cex = 1.2)

## ----titanic-mca1--------------------------------------------------------
titanic.mca <- mjca(Titanic)

## ----titanic-mca2, output.lines=9----------------------------------------
summary(titanic.mca)

## ----titanic-mca3, fig.keep='none', results='hide', echo=FALSE-----------
res <- plot(titanic.mca)

## ----titanic-mca-plot, echo=FALSE, h=6, w=6, out.width='.8\\textwidth', cap='MCA plot of the Titanic data. The category points are joined separately by lines for the factor variables.', fig.pos='!htb'----
# plot, but don't use point labels or points
res <- plot(titanic.mca, labels=0, pch='.', cex.lab=1.2)

# extract factor names and levels
coords <- data.frame(res$cols, titanic.mca$factors)

cols <- c("blue", "red", "brown", "black")
nlev <- c(4,2,2,2)
points(coords[,1:2], pch=rep(16:19, nlev), col=rep(cols, nlev), cex=1.2)
pos <- c(3,1,1,3)
text(coords[,1:2], labels=coords$level, col=rep(cols, nlev), pos=rep(pos,nlev), cex=1.1, xpd=TRUE)

coords <- coords[ order(coords[,"factor"], coords[,"Dim1"]), ]

lines(Dim2 ~ Dim1, data=coords, subset=factor=="Class", lty=1, lwd=2, col="blue")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Sex",  lty=1, lwd=2, col="red")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Age",  lty=1, lwd=2, col="brown")
lines(Dim2 ~ Dim1, data=coords, subset=factor=="Survived",  lty=1, lwd=2, col="black")

legend("topleft", legend=c("Class", "Sex", "Age", "Survived"),
  title="Factor", title.col="black",
	col=cols, text.col=cols, pch=16:19,
	bg="gray95", cex=1.2)

## ----suicide-ca----------------------------------------------------------
suicide.tab <- xtabs(Freq ~ age_sex + method2, data = Suicide)
suicide.ca <- ca(suicide.tab)

## ----ca-suicide-biplot, echo=2, h=6, w=6, out.width='.8\\textwidth', cap='CA biplot of the suicide data using the contribution biplot scaling. Associations between the age--sex categories and the suicide methods can be read as the projections of the points on the vectors. The lengths of the vectors for the suicide categories reflect their contributions to this representation in a 2D plot. ', fig.pos='!htb'----
op <- par(cex=1.3, mar=c(4,4,1,1)+.1, lwd=2)
plot(suicide.ca, map = "colgreen", arrows = c(FALSE, TRUE))
par(op)

## ----cabipl-suicide, eval=FALSE------------------------------------------
## library(UBbipl)
## cabipl(as.matrix(suicide.tab),
##     axis.col = gray(.4), ax.name.size = 1,
##     ca.variant = "PearsonResA",
##     markers = FALSE,
##     row.points.size = 1.5,
##     row.points.col = rep(c("red", "blue"), 4),
##     plot.col.points = FALSE,
##     marker.col = "black", marker.size = 0.8,
##     offset = c(2, 2, 0.5, 0.5),
##     offset.m = rep(-0.2, 14),
##     output = NULL)

## ----biplot-soccer1------------------------------------------------------
data("UKSoccer", package = "vcd")
dimnames(UKSoccer) <- list(Home = paste0("H", 0:4),
                           Away = paste0("A", 0:4))

## ----biplot-soccer2------------------------------------------------------
soccer.pca <- prcomp(log(UKSoccer + 1), center = TRUE, scale. = FALSE)

## ----biplot-soccer-plot, echo=1:2, h=6, w=6, out.width='.7\\textwidth', cap='Biplot for the biadditive representation of independence for the UK soccer scores. The row and column categories are independent in this plot when they appear as points on approximately orthogonal lines.', fig.pos='!htb'----
biplot(soccer.pca, scale = 0, var.axes = FALSE,
  col = c("blue", "red"), cex = 1.2, cex.lab = 1.2,
  xlab = "Dimension 1", ylab = "Dimension 2")

# get the row and column scores
rscores <- soccer.pca$x[,1:2]
cscores <- soccer.pca$rotation[,1:2]
# means, excluding A2 and H2
rmean <- colMeans(rscores[-3,])[2]
cmean <- colMeans(cscores[-3,])[1]

abline(h = rmean, col = "blue", lwd = 2)
abline(v = cmean, col = "red", lwd = 2)
abline(h = 0, lty = 3, col = "gray")
abline(v = 0, lty = 3, col = "gray")

## ----biplot-soccer3, eval=FALSE------------------------------------------
## # get the row and column scores
## rscores <- soccer.pca$x[, 1 : 2]
## cscores <- soccer.pca$rotation[, 1 : 2]
## # means, excluding A2 and H2
## rmean <- colMeans(rscores[-3,])[2]
## cmean <- colMeans(cscores[-3,])[1]
## 
## abline(h = rmean, col = "blue", lwd = 2)
## abline(v = cmean, col = "red", lwd = 2)
## abline(h = 0, lty = 3, col = "gray")
## abline(v = 0, lty = 3, col = "gray")

