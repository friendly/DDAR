## ----parent, include=FALSE-----------------------------------------------
set_parent("example-template.Rnw")

## ----rp0,out.width='0.55\\linewidth', fig.cap='Classification tree for passengers on the \\emph{Titanic}, using \\var{pclass} and \\var{age}.'----
# fit a simple tree, using only pclass and age
library(rpart)
library(rpart.plot)
data(Titanicp, package="vcdExtra")
rp0 <- rpart(survived ~ pclass + age, data=Titanicp)
rpart.plot(rp0, type=0, extra=2, cex=1.5)

## ----rp0-print-----------------------------------------------------------
rp0

## ----partition-map1, echo=FALSE, out.width='0.7\\linewidth', fig.cap='Partition map for the tree in \\figref{fig:rp0}.  Those in the shaded region are predicted to have died; observations are shown by red circles (died) and blue squares (survived)', fig.scap=''----
with(Titanicp, {
  class <- as.numeric(pclass)
  class.jitter <- jitter(class)
	op <- par(mar=c(4,4,0,0)+.2)
	plot(age ~ class.jitter,xlim=c(.6,3.4), type="n", xlab="Class (jittered)", ylab="Age", xaxt="n", cex.lab=1.5)
	axis(1, at=1:3, cex.axis=1.5)
	abline(v=1.5, col="gray")
	abline(v=2.5, col="gray")
	abline(h=16, col="gray")
	points(age[survived=="died"] ~ class.jitter[survived=="died"],pch=15, cex=1.1, col="red")
	points(age[survived=="survived"] ~ class.jitter[survived=="survived"],pch=16, cex=1.1, col="blue")
	rect(1.5,16,2.5,89,col=rgb(0.5,0.5,0.5,1/4))
	rect(2.5,-5,3.61,89,col=rgb(0.5,0.5,0.5,1/4))
  rect(0.48,60,1.5,89, col=rgb(0.5,0.5,0.5,1/4) )
	text(3, 76, "predict: died", cex=1.5, col="red")
	text(1, 8, "predict: survived", cex=1.5, col="blue")
	par(op)
})

## ----rp-plotmo1, out.width='0.7\\linewidth', fig.cap='\\func{plotmo} plot for the tree in \\figref{fig:rp0}. Shading level is proportional to the predicted probability of survival.', fig.scap=''----
library(plotmo)
plotmo(rp0, nresponse="survived", degree1=0, type2="image",
  col.image=gray(seq(.6, 1,.05)),
  col.response=ifelse(Titanicp$survived=="died", "red", "blue"),
  pch=ifelse(Titanicp$survived=="died", 15, 16))

## ----rp-plotmo2, fig.show='hold', out.width='0.32\\linewidth', fig.cap='Other \\func{plotmo} plots: one-way and two-way effects'----
# one-way plots
plotmo(rp0, nresponse="survived", degree1=1, degree2=0, trace=-1, do.par=FALSE)
plotmo(rp0, nresponse="survived", degree1=2, degree2=0, trace=-1, do.par=FALSE)
# two-way, 3D persp plot
plotmo(rp0, nresponse="survived", degree1=0, trace=-1)

## ----printcp-rp0---------------------------------------------------------
printcp(rp0)

## ----plotcp-rp0,out.width='0.6\\linewidth', fig.cap='Plot of complexity and error statistics. The dashed horizontal line is drawn 1 SE above the minimum of the curve.'----
plotcp(rp0, lty=2, col="red", lwd=2)

## ----rp0-pruned,out.width='0.6\\linewidth', fig.cap='Classification tree for passengers on the Titanic, pruned'----
rp0.pruned <- prune(rp0, cp=.05)
rpart.plot(rp0.pruned, type=0, extra=2, cex=1.5, 
           under=TRUE, box.col=c("pink", "lightblue")[rp0.pruned$frame$yval])

## ----titanic-rp1---------------------------------------------------------
rp1 = rpart(survived ~ pclass + sex + age + sibsp, data=Titanicp)

## ----cptable-------------------------------------------------------------
printcp(rp1)

## ----rp1, out.width='.7\\linewidth', fig.cap='Plot of the extended \\func{rpart} tree for four predictors'----
rpart.plot(rp1, type=4, extra=2, faclen=0, under=TRUE, cex=1.1,
  box.col=c("pink", "lightblue")[rp1$frame$yval])

## ----ctree---------------------------------------------------------------
library(party)
titanic.ctree = ctree(survived ~ pclass + sex + age, data=Titanicp)
titanic.ctree

## ----titanic-ctree, out.width='\\linewidth', fig.height=6, fig.width=11, fig.cap='A conditional inference tree for survival on the Titanic. The barplots below each leaf node highlight the proportion of survivors in each branch.'----
plot(titanic.ctree, 
  tp_args = list(fill = c("blue", "lightgray")),
  ip_args = list(fill = c("lightgreen"))
	)


## ----wrapup, include=FALSE, eval=TRUE------------------------------------
pkglist <- setdiff(.packages(), 
        c("knitr", "stats", "graphics", "grDevices", "utils", "datasets", 
          "methods", "base"))
write_bib(pkglist, "ex-packages.bib")

