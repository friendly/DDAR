## ----echo=FALSE----------------------------------------------------------
source("Rprofile.R")
knitrSet("ch02")
#knitrSet("ch02", cache=TRUE)
require(vcdExtra, quietly = TRUE, warn.conflicts = FALSE)  # should go in Rprofile
.locals$ch02 <- NULL
.pkgs$ch02 <- NULL

## ----vectors-matrices, child="ch02/basic.Rnw"----------------------------

## ----vec1----------------------------------------------------------------
c(17, 20, 15, 40)
c("female", "male", "female", "male")
c(TRUE, TRUE, FALSE, FALSE)

## ----vec2----------------------------------------------------------------
count <- c(17, 20, 15, 40)                       # assign
count                                            # print
(sex <- c("female", "male", "female", "male"))   # both
(passed <- c(TRUE, TRUE, FALSE, FALSE))

## ----seq-rep-------------------------------------------------------------
seq(10, 100, by = 10)          # give interval
seq(0, 1, length.out = 11)     # give length
(sex <- rep(c("female", "male"), times = 2))
(sex <- rep(c("female", "male"), length.out = 4))  # same
(passed <- rep(c(TRUE, FALSE), each = 2))

## ----mat1----------------------------------------------------------------
(matA <- matrix(1:8, nrow = 2, ncol = 4))
(matB <- matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE))
(matC <- matrix(1:4, nrow = 2, ncol = 4))

## ----mat2----------------------------------------------------------------
dim(matA)
str(matA)
dimnames(matA) <- list(c("M", "F"), LETTERS[1:4])
matA
str(matA)

## ----mat3----------------------------------------------------------------
dimnames(matA) <- list(sex = c("M", "F"), group = LETTERS[1:4])
## or: names(dimnames(matA)) <- c("Sex", "Group")
matA
str(matA)

## ----mat4----------------------------------------------------------------
rbind(matA, c(10, 20))
cbind(matA, c(10, 20))

## ----mat5----------------------------------------------------------------
t(matA)

## ----mat6----------------------------------------------------------------
2 * matA / 100

## ----array1--------------------------------------------------------------
dims <-  c(2, 4, 2)
(arrayA <- array(1:16, dim = dims))      # 2 rows, 4 columns, 2 layers
str(arrayA)
(arrayB <- array(1:16, dim = c(2, 8)))   # 2 rows, 8 columns
str(arrayB)

## ----array2--------------------------------------------------------------
dimnames(arrayA) <- list(sex = c("M", "F"),
                         group = letters[1:4],
                         time = c("Pre", "Post"))
arrayA
str(arrayA)


## ----dataframe1----------------------------------------------------------
set.seed(12345)   # reproducibility
n <- 100
A <- factor(sample(c("a1", "a2"), n, replace = TRUE))
B <- factor(sample(c("b1", "b2"), n, replace = TRUE))
sex <- factor(sample(c("M", "F"), n, replace = TRUE))
age <- round(rnorm(n, mean = 30, sd = 5))
mydata <- data.frame(A, B, sex, age)
head(mydata, 5)
str(mydata)

## ----dataframe2----------------------------------------------------------
mydata[1,2]
mydata$sex
##same as: mydata[,"sex"] or mydata[,3]

## ----arth-read1, size="footnotesize"-------------------------------------
path <- "ch02/Arthritis.csv" ## set path
## for convenience, use path <- file.choose() to retrieve a path 
## then, use file.show(path) to inspect the data format
Arthritis <- read.table(path, header = TRUE, sep = ",")
str(Arthritis)

## ----arth-read2----------------------------------------------------------
levels(Arthritis$Improved)
Arthritis$Improved <- ordered(Arthritis$Improved,
                              levels = c("None", "Some", "Marked"))

## ----arth-setup, echo=FALSE----------------------------------------------
data("Arthritis", package = "vcd")

## ----case-form,size="footnotesize"---------------------------------------
data("Arthritis", package = "vcd")  # load the data
names(Arthritis)      # show the variables
str(Arthritis)        # show the structure
head(Arthritis, 5)    # first 5 observations, same as Arthritis[1:5,]

## ----GSS-data, echo=FALSE, include=FALSE---------------------------------
tmp <- expand.grid(sex = c("female", "male"),
                   party = c("dem", "indep", "rep"))
GSS <- data.frame(tmp, count = c(279, 165, 73, 47, 225, 191))
xtabs(count~sex+party, data=GSS)

## ----frequency-form,results='markup'-------------------------------------
# Agresti (2002), table 3.11, p. 106
tmp <- expand.grid(sex = c("female", "male"),
                   party = c("dem", "indep", "rep"))
tmp

GSS <- data.frame(tmp, count = c(279, 165, 73, 47, 225, 191))
GSS
names(GSS)
str(GSS)
sum(GSS$count)

## ----table-form1---------------------------------------------------------
data("HairEyeColor", package = "datasets")    # load the data
str(HairEyeColor)                # show the structure
dim(HairEyeColor)                # table dimension sizes
dimnames(HairEyeColor)           # variable and level names
sum(HairEyeColor)                # number of cases

## ----table-form2---------------------------------------------------------
GSS.tab <- matrix(c(279, 73, 225,
                    165, 47, 191), 
		  nrow = 2, ncol = 3, byrow = TRUE)
dimnames(GSS.tab) <- list(sex = c("female", "male"),
                          party = c("dem", "indep", "rep"))
GSS.tab

## ----table-form3---------------------------------------------------------
GSS.tab <- as.table(GSS.tab)
str(GSS.tab)

## ----table-form4---------------------------------------------------------
## A 4 x 4 table  Agresti (2002, Table 2.8, p. 57) Job Satisfaction
JobSat <- matrix(c(1, 2, 1, 0,
                   3, 3, 6, 1,
                   10, 10, 14, 9,
                   6, 7, 12, 11), 
		  nrow = 4, ncol = 4)
dimnames(JobSat) <- 
  list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
       satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS"))
JobSat <- as.table(JobSat)
JobSat

## ----relevel1,results='hide',eval=FALSE----------------------------------
## dimnames(JobSat)$income <- c(7.5, 20, 32.5, 60)
## dimnames(JobSat)$satisfaction <- 1:4

## ----relevel2,results='hide',eval=FALSE----------------------------------
## dimnames(JobSat)$income <-
##     paste(1:4, dimnames(JobSat)$income, sep = ":")
## dimnames(JobSat)$satisfaction <-
##     paste(1:4, dimnames(JobSat)$satisfaction, sep = ":")

## ----reorder1------------------------------------------------------------
data("HairEyeColor", package = "datasets")
HEC <- HairEyeColor[, c(1, 3, 4, 2), ]
str(HEC)

## ----reorder3------------------------------------------------------------
str(UCBAdmissions)
# vary along the 2nd, 1st, and 3rd dimension in UCBAdmissions
UCB <- aperm(UCBAdmissions, c(2, 1, 3))    
dimnames(UCB)$Admit <- c("Yes", "No")
names(dimnames(UCB)) <- c("Sex", "Admitted", "Department")
str(UCB)

## ----table1--------------------------------------------------------------
set.seed(12345)   # reproducibility
n <- 100
A <- factor(sample(c("a1", "a2"), n, replace = TRUE))
B <- factor(sample(c("b1", "b2"), n, replace = TRUE))
sex <- factor(sample(c("M", "F"), n, replace = TRUE))
age <- round(rnorm(n, mean = 30, sd = 5))
mydata <- data.frame(A, B, sex, age)

## ----table-ex1-----------------------------------------------------------
# 2-Way Frequency Table
table(mydata$A, mydata$B)           # A will be rows, B will be columns
## same: with(mydata, table(A, B))
(mytab <- table(mydata[,1:2]))      # same

## ----table-ex2-----------------------------------------------------------
margin.table(mytab)      # sum over A & B
margin.table(mytab, 1)   # A frequencies (summed over B)
margin.table(mytab, 2)   # B frequencies (summed over A)
addmargins(mytab)        # show all marginal totals

## ----table-ex2a----------------------------------------------------------
prop.table(mytab)        # cell proportions
prop.table(mytab, 1)     # row proportions
prop.table(mytab, 2)     # column proportions

## ----table-ex3-----------------------------------------------------------
# 3-Way Frequency Table
mytab <- table(mydata[,c("A", "B", "sex")])
ftable(mytab)

## ----xtabs-ex1-----------------------------------------------------------
# 3-Way Frequency Table
mytable <- xtabs(~ A + B + sex, data = mydata)
ftable(mytable)    # print table
summary(mytable)   # chi-squared test of independence

## ----xtabs-ex2,results='markup'------------------------------------------
(GSStab <- xtabs(count ~ sex + party, data = GSS))
summary(GSStab)

## ----ftable1-------------------------------------------------------------
 ftable(UCB)                      # default
#ftable(UCB, row.vars = 1:2)      # same result
 ftable(Admitted + Sex ~ Department, data = UCB)   # formula method

## ----structable----------------------------------------------------------
library(vcd)
structable(HairEyeColor)                   # show the table: default
structable(Hair + Sex ~ Eye, HairEyeColor) # specify col ~ row variables

## ----subset1-------------------------------------------------------------
HairEyeColor[,,"Female"]
##same using index: HairEyeColor[,,2]

## ----subset2-------------------------------------------------------------
apply(HairEyeColor, 3, sum)

## ----subset3-------------------------------------------------------------
HairEyeColor[c("Black", "Brown"), c("Hazel", "Green"),]

## ----subset4-------------------------------------------------------------
hec <- structable(Eye ~ Sex + Hair, data = HairEyeColor)
hec
hec["Male",]
hec[["Male",]]

## ----subset5-------------------------------------------------------------
hec[[c("Male", "Brown"),]]

## ----subset6-------------------------------------------------------------
rows <- Arthritis$Sex == "Female" & Arthritis$Age > 68
cols <- c("Treatment", "Improved")
Arthritis[rows, cols]

## ----subset7-------------------------------------------------------------
subset(Arthritis, Sex == "Female" & Age > 68, 
       select = c(Treatment, Improved))

## ------------------------------------------------------------------------
subset(Arthritis, Sex == "Female" & Age > 68, 
       select = -c(Age, ID))

## ----dayton1-------------------------------------------------------------
data("DaytonSurvey", package = "vcdExtra")
str(DaytonSurvey)
head(DaytonSurvey)

## ----dayton2-------------------------------------------------------------
# data in frequency form: collapse over sex and race
Dayton_ACM_df <- aggregate(Freq ~ cigarette + alcohol + marijuana,
                           data = DaytonSurvey, FUN = sum)
Dayton_ACM_df

## ----dayton3-------------------------------------------------------------
# convert to table form
Dayton_tab <- xtabs(Freq ~ cigarette + alcohol + marijuana + sex + race,
                    data = DaytonSurvey)
structable(cigarette + alcohol + marijuana ~ sex + race, 
           data = Dayton_tab)

## ----dayton4-------------------------------------------------------------
# collapse over sex and race
Dayton_ACM_tab <- apply(Dayton_tab, MARGIN = 1:3, FUN = sum)
Dayton_ACM_tab <- margin.table(Dayton_tab, 1:3)   # same result
structable(cigarette + alcohol ~ marijuana, data = Dayton_ACM_tab)

## ----dayton5, eval=FALSE-------------------------------------------------
## library(plyr)
## Dayton_ACM_df <- ddply(DaytonSurvey, .(cigarette, alcohol, marijuana),
##                        summarise, Freq = sum(Freq))

## ----collapse1-----------------------------------------------------------
# create some sample data in frequency form
set.seed(12345)   # reproducibility
sex <- c("Male", "Female")
age <- c("10-19", "20-29",  "30-39", "40-49", "50-59", "60-69")
education <- c("low", "med", "high")
dat <- expand.grid(sex = sex, age = age, education = education)
counts <- rpois(36, 100)   # random Poisson cell frequencies
dat <- cbind(dat, counts)
# make it into a 3-way table
tab1 <- xtabs(counts ~ sex + age + education, data = dat)
structable(tab1)

## ----collapse2-----------------------------------------------------------
# collapse age to 3 levels, education to 2 levels
tab2 <- collapse.table(tab1,
         age = c("10-29", "10-29",  "30-49", "30-49", "50-69", "50-69"),
         education = c("<high", "<high", "high"))
structable(tab2)

## ----convert-ex1---------------------------------------------------------
as.data.frame(GSStab)

## ----horse.df1-----------------------------------------------------------
str(as.data.frame(HorseKicks))

## ----horse.df2-----------------------------------------------------------
horse.df <- data.frame(nDeaths = as.numeric(names(HorseKicks)),
                       Freq = as.vector(HorseKicks))
str(horse.df)
horse.df

## ----horse.df3-----------------------------------------------------------
weighted.mean(horse.df$nDeaths, weights=horse.df$Freq)

## ----convert-ex2---------------------------------------------------------
Art.tab <- table(Arthritis[,c("Treatment", "Sex", "Improved")])
str(Art.tab)
ftable(Art.tab)

## ----convert-ex3,size="footnotesize"-------------------------------------
library(vcdExtra)
Art.df <- expand.dft(Art.tab)
str(Art.df)

## ----xtable1-------------------------------------------------------------
data("HorseKicks", package = "vcd")
HorseKicks

## ----xtable2-------------------------------------------------------------
library(xtable)
xtable(HorseKicks)

## ----xtable3, eval=FALSE-------------------------------------------------
## xtable(HorseKicks)

## ----xtable4, eval=FALSE, size='small'-----------------------------------
## tab <- as.data.frame(HorseKicks)
## colnames(tab) <- c("nDeaths", "Freq")
## print(xtable(tab), include.rownames = FALSE,
##       include.colnames = TRUE)

## ----xtable5, eval=FALSE, size='small'-----------------------------------
## horsetab <- t(as.data.frame(addmargins(HorseKicks)))
## rownames(horsetab) <- c( "Number of deaths", "Frequency" )
## horsetab <- xtable(horsetab, digits = 0, label="tab:xtable5",
##      caption = "von Bortkiewicz's data on deaths by horse kicks",
##      align = paste0("l|", paste(rep("r", ncol(horsetab)),
##                                 collapse = ""))
##      )
## print(horsetab, include.colnames=FALSE, caption.placement="top")

## ----tv1-----------------------------------------------------------------
tv_data <- read.table(system.file("doc", "extdata", "tv.dat", 
                                  package = "vcdExtra"))
str(tv_data)
head(tv_data, 5)

## ----tv2,eval=FALSE------------------------------------------------------
## tv_data <- read.table("C:/R/data/tv.dat")

## ----tv2bb,eval=FALSE----------------------------------------------------
## tv_data <- read.table(file.choose())

## ----TV-df---------------------------------------------------------------
TV_df <- tv_data
colnames(TV_df) <- c("Day", "Time", "Network", "State", "Freq")
TV_df <- within(TV_df, {
           Day <- factor(Day, 
                         labels = c("Mon", "Tue", "Wed", "Thu", "Fri"))
           Time <- factor(Time)
           Network <- factor(Network)
           State <- factor(State) 
	 })

## ----tv2a----------------------------------------------------------------
TV <- array(tv_data[,5], dim = c(5, 11, 5, 3))
dimnames(TV) <- 
    list(c("Mon", "Tue", "Wed", "Thu", "Fri"),
         c("8:00", "8:15", "8:30", "8:45", "9:00", "9:15", 
           "9:30", "9:45", "10:00", "10:15", "10:30"),
         c("ABC", "CBS", "NBC", "Fox", "Other"),
         c("Off", "Switch", "Persist"))
names(dimnames(TV)) <- c("Day", "Time", "Network", "State")

## ----tv2b,eval=FALSE-----------------------------------------------------
## TV <- xtabs(V5 ~ ., data = tv_data)
## dimnames(TV) <-
##     list(Day = c("Mon", "Tue", "Wed", "Thu", "Fri"),
##          Time = c("8:00", "8:15", "8:30", "8:45", "9:00", "9:15",
##                   "9:30", "9:45", "10:00", "10:15", "10:30"),
##          Network = c("ABC", "CBS", "NBC", "Fox", "Other"),
##          State = c("Off", "Switch", "Persist"))

## ----tv3-----------------------------------------------------------------
TV <- TV[,,1:3,]     # keep only ABC, CBS, NBC
TV <- TV[,,,3]       # keep only Persist -- now a 3 way table
structable(TV)

## ----tv4-----------------------------------------------------------------
TV2 <- collapse.table(TV, 
                      Time = c(rep("8:00-8:59", 4),
                               rep("9:00-9:59", 4), 
			       rep("10:00-10:44", 3)))
structable(Day ~ Time + Network, TV2)

