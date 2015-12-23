## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch09")
#knitrSet("ch09", cache=TRUE)
require(vcdExtra, quietly = TRUE, warn.conflicts = FALSE)  # should go in Rprofile
.locals$ch09 <- NULL
.pkgs$ch09 <- NULL

## ----contrasts-----------------------------------------------------------
options("contrasts")

## ----loglm1, eval=FALSE--------------------------------------------------
## loglin(mytable, margin = list(c(1, 2), c(1, 3), c(2, 3)))

## ----loglm2, eval=FALSE--------------------------------------------------
## loglm(~ (A + B + C)^2, data = mytable)

## ----loglm3, eval=FALSE--------------------------------------------------
## loglm(Freq ~ (A + B + C)^2, data = mydf)

## ----loglm4, eval=FALSE--------------------------------------------------
## glm(Freq ~ (A + B + C)^2, data = mydf, family = poisson)

## ----stres-plot0---------------------------------------------------------
berkeley <- as.data.frame(UCBAdmissions)
berk.glm1 <- glm(Freq ~ Dept * (Gender + Admit), data = berkeley, 
                 family = "poisson")
fit <- fitted(berk.glm1)
hat <- hatvalues(berk.glm1)
stderr <- sqrt(1 - hat)

## ----stres-plot, echo=FALSE, h=6, w=7, out.width='.7\\textwidth', cap='Standard errors of residuals, $\\sqrt{1-h_i}$ decrease with expected frequencies. This plot shows why ordinary Pearson and deviance residuals may be misleading.  The symbol size in the plot is proportional to leverage, $h_i$. Labels abbreviate Department, Gender, and Admit, colored by Admit.'----
op <- par(mar = c(5,4,1,1)+.1)
plot(fit, stderr, cex = 5 * hat,
  ylab = "Std. Error of Residual", xlab = "Fitted Frequency",
  cex.lab = 1.2)
labs <- with(berkeley,
             paste(Dept, substr(Gender, 1, 1), ifelse(Admit == "Admitted", "+", "-"), sep = ""))
             col <- ifelse(berkeley$Admit == "Admitted", "blue", "red")
text(fit, stderr, labs, col = col, cex = 1.2)
par(op)

## ----berk-loglm0---------------------------------------------------------
data("UCBAdmissions")
library(MASS)
berk.loglm0 <- loglm(~ Dept + Gender + Admit, data = UCBAdmissions,
                     param = TRUE, fitted = TRUE)
berk.loglm0

## ----berk-loglm0-1, R.options=list(digits=4)-----------------------------
structable(Dept ~ Admit + Gender, fitted(berk.loglm0))

## ----berk-loglm1---------------------------------------------------------
# conditional independence in UCB admissions data
berk.loglm1 <- loglm(~ Dept * (Gender + Admit), data = UCBAdmissions)
berk.loglm1

## ----berk-loglm2---------------------------------------------------------
berk.loglm2 <-loglm(~ (Admit + Dept + Gender)^2, data = UCBAdmissions)
berk.loglm2

## ----berk-loglm-anova----------------------------------------------------
anova(berk.loglm0, berk.loglm1, berk.loglm2, test = "Chisq")

## ------------------------------------------------------------------------
berkeley <- as.data.frame(UCBAdmissions)
head(berkeley)

## ----berk-glm1-----------------------------------------------------------
berk.glm1 <- glm(Freq ~ Dept * (Gender + Admit),
                 data = berkeley, family = "poisson")

## ----berk-glm2-----------------------------------------------------------
berk.glm2 <- glm(Freq ~ (Dept + Gender + Admit)^2,
                 data = berkeley, family = "poisson")

## ----berk-glm-anova------------------------------------------------------
anova(berk.glm1, berk.glm2, test = "Chisq")

## ----berk-glm1-anova-----------------------------------------------------
anova(berk.glm1, test = "Chisq")

## ----berk-glm1-mosaic, fig.pos='!htb', h=6, w=8, out.width='.7\\textwidth', cap='Mosaic display for the model [AD][GD], showing standardized residuals for the cell contributions to \\GSQ.', scap='Mosaic display for the model AD GD, showing standardized residuals.'----
library(vcdExtra)
mosaic(berk.glm1, shade = TRUE, 
       formula = ~ Dept + Admit + Gender, split = TRUE,
       residuals_type = "rstandard", 
       main = "Model: [AdmitDept][GenderDept]", 
       labeling = labeling_residuals, 
       abbreviate_labs = c(Gender = TRUE),
       keep_aspect_ratio = FALSE)

## ----berk-glm3-----------------------------------------------------------
berkeley <- within(berkeley, 
                   dept1AG <- (Dept == "A") * 
                     (Gender == "Female") * 
                     (Admit == "Admitted"))
head(berkeley)

## ----berk-glm4-----------------------------------------------------------
berk.glm3 <- glm(Freq ~ Dept * (Gender + Admit) + dept1AG, 
                 data = berkeley, family = "poisson")

## ----berk-glm5-----------------------------------------------------------
LRstats(berk.glm3)
anova(berk.glm1, berk.glm3, test = "Chisq")

## ----berk-glm6-----------------------------------------------------------
coef(berk.glm3)[["dept1AG"]]
exp(coef(berk.glm3)[["dept1AG"]])

## ----berk-glm3-mosaic, fig.pos='!htb', h=6, w=8, out.width='.7\\textwidth', cap='Mosaic display for the model \\code{berk.glm3}, allowing an association of gender and admission in Department A. This model now fits the data well.', scap='Mosaic display for the model berk.glm3', echo=FALSE, fig.pos="H"----
mosaic(berk.glm3, shade = TRUE, 
       formula = ~ Dept + Admit + Gender, split = TRUE,
       residuals_type = "rstandard", 
       main = "Model: [DeptGender][DeptAdmit] + DeptA*[GA]",
       labeling = labeling_residuals, 
       abbreviate_labs = c(Gender = TRUE),
       keep_aspect_ratio = FALSE)

## ----berk-glm3-mosaic-code, fig.pos='!htb', h=6, w=8, out.width='.7\\textwidth', cap='Mosaic display for the model \\code{berk.glm3}, allowing an association of gender and admission in Department A. This model now fits the data well.', scap='Mosaic display for the model berk.glm3', eval=FALSE----
## mosaic(berk.glm3, shade = TRUE,
##        formula = ~ Dept + Admit + Gender, split = TRUE,
##        residuals_type = "rstandard",
##        main = "Model: [DeptGender][DeptAdmit] + DeptA*[GA]",
##        labeling = labeling_residuals,
##        abbreviate_labs = c(Gender = TRUE),
##        keep_aspect_ratio = FALSE)

## ----berk-logit1, R.options=list(digits=4)-------------------------------
(obs <- log(UCBAdmissions[1,,] / UCBAdmissions[2,,]))

## ----berk-logit2---------------------------------------------------------
berk.logit2 <- glm(Admit == "Admitted" ~ Dept + Gender,
                   data = berkeley, weights = Freq, family = "binomial")
summary(berk.logit2)

## ----berk-logit3---------------------------------------------------------
berkeley <- within(berkeley,
                   dept1AG <- (Dept == "A") * (Gender == "Female"))
berk.logit3 <- glm(Admit == "Admitted" ~ Dept + Gender + dept1AG,
                   data = berkeley, weights = Freq, family = "binomial")

## ----berk-anova, R.options=list(digits=6)--------------------------------
library(car)
Anova(berk.logit2)
Anova(berk.logit3)

## ----berk-pred2----------------------------------------------------------
pred2 <- cbind(berkeley[,1:3], fit = predict(berk.logit2))
pred2 <- cbind(subset(pred2, Admit == "Admitted"), obs = as.vector(obs))
head(pred2)

## ----berk-logit-plot, eval=FALSE, fig.show='hide'------------------------
## library(ggplot2)
## ggplot(pred2, aes(x = Dept, y = fit, group = Gender, color = Gender)) +
##   geom_line(size = 1.2) +
##   geom_point(aes(x = Dept, y = obs, group = Gender, color = Gender),
##              size = 4) +
##   ylab("Log odds (Admitted)") + theme_bw() +
##   theme(legend.position = c(.8, .9),
##         legend.title = element_text(size = 14),
##         legend.text = element_text(size = 14))

## ----health1-------------------------------------------------------------
Health <- expand.grid(concerns = c("sex", "menstrual",
                                   "healthy", "nothing"),
                      age      = c("12-15", "16-17"),
                      gender   = c("M", "F"))
Health$Freq <- c(4, 0, 42, 57, 2, 0, 7, 20,
                 9, 4, 19, 71, 7, 8, 10, 21)

## ----health2-------------------------------------------------------------
health.glm0 <- glm(Freq ~ concerns + age + gender, data = Health,
                   subset = (Freq > 0), family = poisson)
health.glm1 <- glm(Freq ~ concerns + age * gender, data = Health,
                   subset = (Freq > 0), family = poisson)

## ----health3-------------------------------------------------------------
LRstats(health.glm0, health.glm1)

## ----health-mosaic, h=6, w=8, out.width='.6\\textwidth', cap='Mosaic display for the Health data, model \\code{health.glm1}.', scap='Mosaic display for the Health data, model health.glm1'----
mosaic(health.glm1, ~ concerns + age + gender, 
       residuals_type = "rstandard", 
       rot_labels = 0, 
       just_labels = c(left = "right"),
       margin = c(left = 5))

## ----health4-------------------------------------------------------------
health.glm2 <- glm(Freq ~ concerns*gender + concerns*age, data = Health,
                   subset = (Freq > 0), family = poisson)
LRstats(health.glm2)

## ----health5, R.options=list(width=80), size="footnotesize"--------------
summary(health.glm2)

## ----health-loglin1------------------------------------------------------
health.tab <- xtabs(Freq ~ concerns + age + gender, data = Health)

## ----health-loglm2-------------------------------------------------------
nonzeros <- ifelse(health.tab>0, 1, 0)
health.loglm0 <- loglm(~ concerns + age + gender,
                       data = health.tab, start = nonzeros)
health.loglm1 <- loglm(~ concerns + age * gender,
                       data = health.tab, start = nonzeros)
# df is wrong
health.loglm2 <- loglm(~ concerns*gender + concerns*age,
                       data = health.tab, start = nonzeros)
LRstats(health.loglm0, health.loglm1, health.loglm2)

