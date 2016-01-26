### 2. Working with Categorical Data
### 2.1. Working with R data: vectors, matrices, arrays, and data frames

### 2.1.1. Vectors

library(vcdExtra)

## ceating simple vectors
c(17, 20, 15, 40)
c("female", "male", "female", "male")
c(TRUE, TRUE, FALSE, FALSE)

## storing vectors
count <- c(17, 20, 15, 40)                       # assign
count                                            # print
(sex <- c("female", "male", "female", "male"))   # both
(passed <- c(TRUE, TRUE, FALSE, FALSE))

## (repeated) sequences
seq(10, 100, by = 10)          # give interval
seq(0, 1, length.out = 11)     # give length
(sex <- rep(c("female", "male"), times = 2))
(sex <- rep(c("female", "male"), length.out = 4))  # same
(passed <- rep(c(TRUE, FALSE), each = 2))

### 2.1.2. Matrices

## creating matrices
(matA <- matrix(1:8, nrow = 2, ncol = 4))
(matB <- matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE))
(matC <- matrix(1:4, nrow = 2, ncol = 4))

## dimension attribute and labels for matrices
dim(matA)
str(matA)
dimnames(matA) <- list(c("M", "F"), LETTERS[1:4])
matA
str(matA)

## names for dimension names
dimnames(matA) <- list(sex = c("M", "F"), group = LETTERS[1:4])
## or: names(dimnames(matA)) <- c("Sex", "Group")
matA
str(matA)

## adding rows and columns to matrices
rbind(matA, c(10, 20))
cbind(matA, c(10, 20))

## transposing matrices
t(matA)

## calculations with matrices
2 * matA / 100

### 2.1.3. Arrays

## creating arrays
dims <-  c(2, 4, 2)
(arrayA <- array(1:16, dim = dims))      # 2 rows, 4 columns, 2 layers
str(arrayA)
(arrayB <- array(1:16, dim = c(2, 8)))   # 2 rows, 8 columns
str(arrayB)

## labels for arrays
dimnames(arrayA) <- list(sex = c("M", "F"),
                         group = letters[1:4],
                         time = c("Pre", "Post"))
arrayA
str(arrayA)

### 2.1.4. data frames

## creating data frames
set.seed(12345)   # reproducibility
n <- 100
A <- factor(sample(c("a1", "a2"), n, replace = TRUE))
B <- factor(sample(c("b1", "b2"), n, replace = TRUE))
sex <- factor(sample(c("M", "F"), n, replace = TRUE))
age <- round(rnorm(n, mean = 30, sd = 5))
mydata <- data.frame(A, B, sex, age)
head(mydata, 5)
str(mydata)

## subsetting data frames
mydata[1,2]
mydata$sex
#same as: mydata[,"sex"] or mydata[,3]

## Example 2.1.: Arthritis Treatment
library(vcd)
# create data file for this example
write.table(Arthritis, file = "Arthritis.csv", quote = FALSE, sep = ",")
path <- "Arthritis.csv" # set path
# for convenience, use path <- file.choose() to retrieve a path
# then, use file.show(path) to inspect the data format
Arthritis <- read.table(path, header = TRUE, sep = ",")
str(Arthritis)

## make Improved an ordered factor
levels(Arthritis$Improved)
Arthritis$Improved <- ordered(Arthritis$Improved,
                              levels = c("None", "Some", "Marked"))

### 2.2. Forms of categorical data: case form, frequency form, and table form

### 2.2.1. Case Form

## Example 2.2.: Arthritis Treatment

data("Arthritis", package = "vcd")  # load the data
names(Arthritis)      # show the variables
str(Arthritis)        # show the structure
head(Arthritis, 5)    # first 5 observations, same as Arthritis[1:5,]

## Example 2.3.: General social survey

## creating GSS data in frequency form
tmp <- expand.grid(sex = c("female", "male"),
                   party = c("dem", "indep", "rep"))
tmp

GSS <- data.frame(tmp, count = c(279, 165, 73, 47, 225, 191))
GSS
names(GSS)
str(GSS)
sum(GSS$count)

### 2.2.3. Table form

## Example 2.4.: Hair and Eye color
data("HairEyeColor", package = "datasets")    # load the data
str(HairEyeColor)                # show the structure
dim(HairEyeColor)                # table dimension sizes
dimnames(HairEyeColor)           # variable and level names
sum(HairEyeColor)                # number of cases

## GSS data as a matrix
GSS.tab <- matrix(c(279, 73, 225,
                    165, 47, 191),
		  nrow = 2, ncol = 3, byrow = TRUE)
dimnames(GSS.tab) <- list(sex = c("female", "male"),
                          party = c("dem", "indep", "rep"))
GSS.tab

## transforming a matrix to a table
GSS.tab <- as.table(GSS.tab)
str(GSS.tab)

## Example 2.5.: Job Satisfaction
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

### 2.3. Ordered factors and reordered tables

## Assigning numeric values as labels
dimnames(JobSat)$income <- c(7.5, 20, 32.5, 60)
dimnames(JobSat)$satisfaction <- 1:4

## Assigning a prefix to labels
dimnames(JobSat)$income <-
     paste(1:4, dimnames(JobSat)$income, sep = ":")
dimnames(JobSat)$satisfaction <-
     paste(1:4, dimnames(JobSat)$satisfaction, sep = ":")

## Permuting labels
data("HairEyeColor", package = "datasets")
HEC <- HairEyeColor[, c(1, 3, 4, 2), ]
str(HEC)

## Permuting dimensions
str(UCBAdmissions)
# vary along the 2nd, 1st, and 3rd dimension in UCBAdmissions
UCB <- aperm(UCBAdmissions, c(2, 1, 3))
dimnames(UCB)$Admit <- c("Yes", "No")
names(dimnames(UCB)) <- c("Sex", "Admitted", "Department")
str(UCB)

### 2.4. Generating tables with table() and xtabs()

## Creating sample data
set.seed(12345)   # reproducibility
n <- 100
A <- factor(sample(c("a1", "a2"), n, replace = TRUE))
B <- factor(sample(c("b1", "b2"), n, replace = TRUE))
sex <- factor(sample(c("M", "F"), n, replace = TRUE))
age <- round(rnorm(n, mean = 30, sd = 5))
mydata <- data.frame(A, B, sex, age)

### 2.4.1. table()

## Creating a 2-Way Frequency Table
table(mydata$A, mydata$B)           # A will be rows, B will be columns
# same: with(mydata, table(A, B))
(mytab <- table(mydata[,1:2]))      # same

## marginal sums
margin.table(mytab)      # sum over A & B
margin.table(mytab, 1)   # A frequencies (summed over B)
margin.table(mytab, 2)   # B frequencies (summed over A)
addmargins(mytab)        # show all marginal totals

## proportions
prop.table(mytab)        # cell proportions
prop.table(mytab, 1)     # row proportions
prop.table(mytab, 2)     # column proportions

## 3-Way Frequency Table
mytab <- table(mydata[,c("A", "B", "sex")])
ftable(mytab)

### 2.4.2. xtabs()

## 3-Way Frequency Table
mytable <- xtabs(~ A + B + sex, data = mydata)
ftable(mytable)    # print table
summary(mytable)   # chi-squared test of independence

## Pretabulated data
(GSStab <- xtabs(count ~ sex + party, data = GSS))
summary(GSStab)

### 2.5. Printing tables with structable() and ftable()

### 2.5.1. Text output

## ftable()
 ftable(UCB)                      # default
#ftable(UCB, row.vars = 1:2)      # same result
 ftable(Admitted + Sex ~ Department, data = UCB)   # formula method

## structable()
library(vcd)
structable(HairEyeColor)                   # show the table: default
structable(Hair + Sex ~ Eye, HairEyeColor) # specify col ~ row variables

### 2.6. Subsetting data

### 2.6.1. Subsetting tables

## Extracting female data from HairEyeColor data
HairEyeColor[,,"Female"]
##same using index: HairEyeColor[,,2]

## Applying functions to subsets of the data
apply(HairEyeColor, 3, sum)

## Subsetting with more than one level
HairEyeColor[c("Black", "Brown"), c("Hazel", "Green"),]

### 2.6.2. Subsetting structables

hec <- structable(Eye ~ Sex + Hair, data = HairEyeColor)
hec
hec["Male",]
hec[["Male",]]

## Subsetting with more than one level
hec[[c("Male", "Brown"),]]

### 2.6.3. Subsetting data frames

## subsetting using indexes
rows <- Arthritis$Sex == "Female" & Arthritis$Age > 68
cols <- c("Treatment", "Improved")
Arthritis[rows, cols]

## subsetting using subset
subset(Arthritis, Sex == "Female" & Age > 68,
       select = c(Treatment, Improved))

## removing columns using subset
subset(Arthritis, Sex == "Female" & Age > 68,
       select = -c(Age, ID))

### 2.7. Collapsing tables

### 2.7.1. Collapsing over factor tables

## Example 2.6.: Dayton survey
data("DaytonSurvey", package = "vcdExtra")
str(DaytonSurvey)
head(DaytonSurvey)

## data in frequency form: collapse over sex and race
Dayton_ACM_df <- aggregate(Freq ~ cigarette + alcohol + marijuana,
                           data = DaytonSurvey, FUN = sum)
Dayton_ACM_df

## convert to table form
Dayton_tab <- xtabs(Freq ~ cigarette + alcohol + marijuana + sex + race,
                    data = DaytonSurvey)
structable(cigarette + alcohol + marijuana ~ sex + race,
           data = Dayton_tab)

## collapse over sex and race
Dayton_ACM_tab <- apply(Dayton_tab, MARGIN = 1:3, FUN = sum)
Dayton_ACM_tab <- margin.table(Dayton_tab, 1:3)   # same result
structable(cigarette + alcohol ~ marijuana, data = Dayton_ACM_tab)

## using the plyr package
library(plyr)
Dayton_ACM_df <- ddply(DaytonSurvey, .(cigarette, alcohol, marijuana),
                       summarise, Freq = sum(Freq))

### 2.7.2. Collapsing table levels

## Example 2.8.: Collapsing categories

## create some sample data in frequency form
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

## collapse age to 3 levels, education to 2 levels
tab2 <- collapse.table(tab1,
         age = c("10-29", "10-29",  "30-49", "30-49", "50-69", "50-69"),
         education = c("<high", "<high", "high"))
structable(tab2)

### 2.8. Converting among frequency tables and data frames

### 2.8.1. Table form to frequency form

## Example 2.9.: General social survey
as.data.frame(GSStab)

## Example 2.10.: Death by horse kick
str(as.data.frame(HorseKicks))

## coercing table names to numbers ...
horse.df <- data.frame(nDeaths = as.numeric(names(HorseKicks)),
                       Freq = as.vector(HorseKicks))
str(horse.df)
horse.df

## ... and applying weighted.mean()
weighted.mean(horse.df$nDeaths, weights=horse.df$Freq)

### 2.8.2. Case form to table form

## Example 2.11. Arthritis treatment
Art.tab <- table(Arthritis[,c("Treatment", "Sex", "Improved")])
str(Art.tab)
ftable(Art.tab)

### 2.8.3. Table form to case form

## Example 2.12.: Arthritis treatment
library(vcdExtra)
Art.df <- expand.dft(Art.tab)
str(Art.df)

### 2.8.4. Publishing tables to LaTeX or HTML

## the horsekicks data
data("HorseKicks", package = "vcd")
HorseKicks

## using xtable()
library(xtable)
xtable(HorseKicks)

## modified output using xtable()
tab <- as.data.frame(HorseKicks)
colnames(tab) <- c("nDeaths", "Freq")
print(xtable(tab), include.rownames = FALSE,
      include.colnames = TRUE)

## Table 2.2.: horsekicks data in transposed form using xtable()
horsetab <- t(as.data.frame(addmargins(HorseKicks)))
rownames(horsetab) <- c( "Number of deaths", "Frequency" )
horsetab <- xtable(horsetab, digits = 0, label="tab:xtable5",
     caption = "von Bortkiewicz's data on deaths by horse kicks",
     align = paste0("l|", paste(rep("r", ncol(horsetab)),
                                collapse = ""))
     )
print(horsetab, include.colnames=FALSE, caption.placement="top")

### 2.9. A complex example: TV viewing data

### 2.9.1. Creating data frames and arrays

## reading in the data
tv_data <- read.table(system.file("doc", "extdata", "tv.dat",
                                  package = "vcdExtra"))
str(tv_data)
head(tv_data, 5)

## tv_data <- read.table("C:/R/data/tv.dat")

## tv_data <- read.table(file.choose())

## creating factors within the data frame
TV_df <- tv_data
colnames(TV_df) <- c("Day", "Time", "Network", "State", "Freq")
TV_df <- within(TV_df, {
           Day <- factor(Day,
                         labels = c("Mon", "Tue", "Wed", "Thu", "Fri"))
           Time <- factor(Time)
           Network <- factor(Network)
           State <- factor(State)
	 })

## reshaping the table into a 4-way table
TV <- array(tv_data[,5], dim = c(5, 11, 5, 3))
dimnames(TV) <-
    list(c("Mon", "Tue", "Wed", "Thu", "Fri"),
         c("8:00", "8:15", "8:30", "8:45", "9:00", "9:15",
           "9:30", "9:45", "10:00", "10:15", "10:30"),
         c("ABC", "CBS", "NBC", "Fox", "Other"),
         c("Off", "Switch", "Persist"))
names(dimnames(TV)) <- c("Day", "Time", "Network", "State")

## Creating the table using xtabs()
TV <- xtabs(V5 ~ ., data = tv_data)
dimnames(TV) <-
    list(Day = c("Mon", "Tue", "Wed", "Thu", "Fri"),
         Time = c("8:00", "8:15", "8:30", "8:45", "9:00", "9:15",
                  "9:30", "9:45", "10:00", "10:15", "10:30"),
         Network = c("ABC", "CBS", "NBC", "Fox", "Other"),
         State = c("Off", "Switch", "Persist"))

### 2.9.2. Subsetting and collapsing

## subsetting data
TV <- TV[,,1:3,]     # keep only ABC, CBS, NBC
TV <- TV[,,,3]       # keep only Persist -- now a 3 way table
structable(TV)

## collapsing time labels
TV2 <- collapse.table(TV,
                      Time = c(rep("8:00-8:59", 4),
                               rep("9:00-9:59", 4),
			       rep("10:00-10:44", 3)))
structable(Day ~ Time + Network, TV2)

