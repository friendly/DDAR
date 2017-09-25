## ----parent, include=FALSE-----------------------------------------------
set_parent("example-template.Rnw")

## ----setup, include=FALSE------------------------------------------------
library(vcdExtra)
library(car)
# data(Titanicp)

## ----load-data-----------------------------------------------------------
data(Titanicp, package="vcdExtra")
Titanicp <- Titanicp[!is.na(Titanicp$age),]

## ----titanic-glm-ggp1, out.width='.6\\linewidth', fig.height=4, fig.pos='hbt!', fig.cap='Survival on the Titanic, by age and sex, with a \\code{loess} smooth and 95\\% confidence band'----
require(ggplot2)
ggplot(Titanicp, aes(age, as.numeric(survived)-1, color=sex)) +
  stat_smooth(method="loess", formula=y~x, 
              alpha=0.2, size=2, aes(fill=sex)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Age") + ylab("Pr (survived)")  

## ----titanic-glm-ggp2, out.width='.6\\linewidth', fig.height=4, fig.pos='hb', fig.cap='Survival on the Titanic, by age and sex, with a \\code{glm} smooth and 95\\% confidence band'----
ggplot(Titanicp, aes(age, as.numeric(survived)-1, color=sex)) +
  stat_smooth(method="glm", family=binomial, formula=y~x, 
              alpha=0.2, size=2, aes(fill=sex)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Age") + ylab("Pr (survived)")  

## ----titanic-glm-ggp-logit, out.width='.6\\linewidth', fig.height=4, fig.pos='hb', fig.cap='Survival on the Titanic, by age and sex plotted on the log odds scale where they are linear'----
logit <- function(x) log(x)/log(1-x)
ggplot(Titanicp, aes(age, as.numeric(survived)-1, color=sex)) +
  stat_smooth(method="glm", family=binomial, formula=y~x, 
              alpha=0.2, size=2, aes(fill=sex)) +
  scale_y_continuous(breaks=c(.10, .25, .50, .75, .90)) +
  coord_trans(y="logit") + xlab("Age") + ylab("Pr (survived)")

## ----titanic-glm-ggp3, out.width='\\linewidth', fig.height=3.5, fig.pos='hb', fig.cap='Survival on the Titanic, by age and sex, with panels for passenger class'----
p <- ggplot(Titanicp, aes(age, as.numeric(survived)-1, color=sex)) +
  stat_smooth(method="glm", family=binomial, formula=y~x, 
              alpha=0.2, size=2, aes(fill=sex)) +
	geom_point(position=position_jitter(height=0.03, width=0)) +
	xlab("Age") + ylab("Pr (survived)")	

# facet by pclass
p + facet_grid(. ~ pclass) 


## ----eval=FALSE----------------------------------------------------------
## # add plot collapsed over pclass
## p + facet_grid(. ~ pclass, margins=TRUE)
## 

## ----titanic-glm-ggp4, out.width='\\linewidth', fig.height=3.5, fig.cap='Survival on the Titanic, by age and passenger class, with panels for passenger sex'----
# facet by sex, curves by class
p <- ggplot(Titanicp, aes(age, as.numeric(survived)-1, color=pclass)) +
  stat_smooth(method="glm", family=binomial, formula=y~x, 
              alpha=0.2, size=2, aes(fill=pclass)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
	xlab("Age") + ylab("Pr (survived)")	

# facet by sex
p + facet_grid(. ~ sex) 

## ----glm-models----------------------------------------------------------
titanic.glm1 <- glm(survived ~ pclass + sex + age, data=Titanicp, family=binomial)
titanic.glm2 <- glm(survived ~ (pclass + sex + age)^2, data=Titanicp, family=binomial)
titanic.glm3 <- glm(survived ~ (pclass + sex + age)^3, data=Titanicp, family=binomial)
anova(titanic.glm1, titanic.glm2, titanic.glm3, test="Chisq")

## ------------------------------------------------------------------------
vcdExtra::LRstats(glmlist(titanic.glm1, titanic.glm2, titanic.glm3))

## ----summary-titanic-glm2------------------------------------------------
summary(titanic.glm2)

## ----contrasts-pclass----------------------------------------------------
contrasts(Titanicp$pclass)

## ----Anova-titanic-glm2--------------------------------------------------
Anova(titanic.glm2)

## ----titanic-eff2--------------------------------------------------------
library(effects)
titanic.eff2 <- allEffects(titanic.glm2)
names(titanic.eff2)

## ----titanic-eff2a-------------------------------------------------------
titanic.eff2a <- allEffects(titanic.glm2, 
  typical=median,
  given.values=c(pclass2nd=1/3, pclass3rd=1/3, sexmale=0.5)
	)

## ----titanic-eff2-menu, eval=FALSE---------------------------------------
## plot(titanic.eff2, ask=TRUE, ...)

## ----titanic-eff2-1,out.width = '.5\\linewidth'--------------------------
ticks <- list(at=c(.01, .05, seq(.1, .9, by=.2), .95, .99))
plot(titanic.eff2[1], ticks=ticks, multiline=TRUE, ci.style="bars", key=list(x=.7, y=.95))

## ----titanic-eff2-2, fig.show='hide'-------------------------------------
plot(titanic.eff2[2], ticks=ticks, multiline=TRUE, ci.style="bars", key=list(x=.7, y=.95))

## ----titanic-eff2-3, fig.show='hide'-------------------------------------
plot(titanic.eff2[3], ticks=ticks, multiline=TRUE, ci.style="bars", key=list(x=.7, y=.95))

## ----wrapup, include=FALSE, eval=TRUE------------------------------------
pkglist <- setdiff(.packages(), 
        c("knitr", "stats", "graphics", "grDevices", "utils", "datasets", 
          "methods", "base"))
#write_bib(pkglist, "ex-packages.bib")

