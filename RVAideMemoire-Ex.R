pkgname <- "RVAideMemoire"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('RVAideMemoire')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("bootstrap")
### * bootstrap

flush(stderr()); flush(stdout())

### Name: bootstrap
### Title: Bootstrap
### Aliases: bootstrap

### ** Examples

# Confidence interval of a mean
samp <- sample(1:50,10,replace=TRUE)
bootstrap(samp,function(x,i) mean(x[i]))

# Confidence interval of the standard error of the mean
bootstrap(samp,function(x,i) sd(x[i])/sqrt(length(x[i])))



cleanEx()
nameEx("chisq.bintest")
### * chisq.bintest

flush(stderr()); flush(stdout())

### Name: chisq.bintest
### Title: Pearson's Chi-squared test or Fisher's exact test for binary
###   variables
### Aliases: chisq.bintest

### ** Examples

response <- c(0,0,0,0,0,0,1,0,0,1,0,0,1,0,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1)
fact <- gl(3,10,labels=LETTERS[1:3])
chisq.bintest(response~fact)



cleanEx()
nameEx("chisq.exp")
### * chisq.exp

flush(stderr()); flush(stdout())

### Name: chisq.exp
### Title: Expected counts for comparison of proportions to given values
### Aliases: chisq.exp

### ** Examples

proportions <- sample(c(0,1),60,replace=TRUE)
populations <- sample(LETTERS[1:3],60,replace=TRUE)
tab.cont <- table(populations,proportions)
p.theo <- c(0.2,0.5,0.7)
chisq.exp(tab.cont,p=p.theo)



cleanEx()
nameEx("chisq.multcomp")
### * chisq.multcomp

flush(stderr()); flush(stdout())

### Name: chisq.multcomp
### Title: Pairwise comparisons after a chi-squared goodness-of-fit test
### Aliases: chisq.multcomp

### ** Examples

counts <- c(5,15,23,8,14)
chisq.test(counts)
chisq.multcomp(counts)



cleanEx()
nameEx("chisq.theo.multcomp")
### * chisq.theo.multcomp

flush(stderr()); flush(stdout())

### Name: chisq.theo.multcomp
### Title: Pairwise comparisons after a chi-squared test for given
###   probabilities
### Aliases: chisq.theo.multcomp

### ** Examples

counts <- c(5,15,23,8,14)
p.theo <- c(0.1,0.4,0.3,0.15,0.05)
chisq.test(counts,p=p.theo)
chisq.theo.multcomp(counts,p=p.theo)



cleanEx()
nameEx("cochran.qtest")
### * cochran.qtest

flush(stderr()); flush(stdout())

### Name: cochran.qtest
### Title: Cochran's Q test
### Aliases: cochran.qtest

### ** Examples

response <- c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1)
fact <- gl(3,1,30,labels=LETTERS[1:3])
block <- gl(10,3,labels=letters[1:10])
cochran.qtest(response~fact|block)



cleanEx()
nameEx("cor.2comp")
### * cor.2comp

flush(stderr()); flush(stdout())

### Name: cor.2comp
### Title: Comparison of 2 Pearson's linear correlation coefficients
### Aliases: cor.2comp

### ** Examples

cor1.var1 <- 1:30+rnorm(30,0,2)
cor1.var2 <- 1:30+rnorm(30,0,3)
cor2.var1 <- (-1):-30+rnorm(30,0,2)
cor2.var2 <- (-1):-30+rnorm(30,0,3)
cor.2comp(cor1.var1,cor1.var2,cor2.var1,cor2.var2)



cleanEx()
nameEx("cor.conf")
### * cor.conf

flush(stderr()); flush(stdout())

### Name: cor.conf
### Title: Equality of a Pearson's linear correlation coefficient to a
###   given value
### Aliases: cor.conf

### ** Examples

var1 <- 1:30+rnorm(30,0,4)
var2 <- 1:30+rnorm(30,0,4)
cor.conf(var1,var2,theo=0.5)



cleanEx()
nameEx("cor.multcomp")
### * cor.multcomp

flush(stderr()); flush(stdout())

### Name: cor.multcomp
### Title: Comparison of several Pearson's linear correlation coefficients
### Aliases: cor.multcomp

### ** Examples

var1 <- c(1:15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
var2 <- c(-1:-15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
fact <- gl(3,15,labels=LETTERS[1:3])
cor.multcomp(var1,var2,fact)

var3 <- c(1:15+rnorm(15,0,1),1:15+rnorm(15,0,3),1:15+rnorm(15,0,2))
cor.multcomp(var1,var3,fact)



cleanEx()
nameEx("cox.resid")
### * cox.resid

flush(stderr()); flush(stdout())

### Name: cox.resid
### Title: Martingale residuals of a Cox model
### Aliases: cox.resid

### ** Examples

# 'kidney' dataset in MASS package
data(kidney)
model <- coxph(Surv(time,status)~age+factor(sex),data=kidney)
cox.resid(model)



cleanEx()
nameEx("cramer.coeff")
### * cramer.coeff

flush(stderr()); flush(stdout())

### Name: cramer.coeff
### Title: Cramer's association coefficient
### Aliases: cramer.coeff

### ** Examples

var1 <- sample(LETTERS[1:3],30,replace=TRUE)
var2 <- sample(letters[1:3],30,replace=TRUE)
cramer.coeff(var1,var2)



cleanEx()
nameEx("cv")
### * cv

flush(stderr()); flush(stdout())

### Name: cv
### Title: Coefficient of variation
### Aliases: cv

### ** Examples

cv(rnorm(30))



cleanEx()
nameEx("fisher.multcomp")
### * fisher.multcomp

flush(stderr()); flush(stdout())

### Name: fisher.multcomp
### Title: Pairwise comparisons after a test for independence of 2
###   categorical variables
### Aliases: fisher.multcomp

### ** Examples

tab.cont <- as.table(matrix(c(25,10,12,6,15,14,9,16,9),ncol=3,dimnames=list(c("fair",
  "dark","russet"),c("blue","brown","green"))))
chisq.test(tab.cont)
fisher.multcomp(tab.cont)



cleanEx()
nameEx("ind.contrib")
### * ind.contrib

flush(stderr()); flush(stdout())

### Name: ind.contrib
### Title: Individual contributions in regression
### Aliases: ind.contrib

### ** Examples

x <- 1:30
y <- 1:30+rnorm(30,0,4)
model1 <- lm(y~x)
model2 <- least.rect(y~x)
ind.contrib(model1)
ind.contrib(model2)



cleanEx()
nameEx("least.rect")
### * least.rect

flush(stderr()); flush(stdout())

### Name: least.rect
### Title: Least rectangles linear regression
### Aliases: least.rect

### ** Examples

x <- 1:30+rnorm(30,0,3)
y <- 1:30+rnorm(30,0,3)
least.rect(y~x)



cleanEx()
nameEx("logis.fit")
### * logis.fit

flush(stderr()); flush(stdout())

### Name: logis.fit
### Title: Graphical adujstment of a simple binary logistic regression to
###   data
### Aliases: logis.fit

### ** Examples

x <- 1:50
y <- c(rep(0,18),sample(0:1,14,replace=TRUE),rep(1,18))
model <- glm(y~x,family=binomial)
plot(x,y)
lines(x,model$fitted)
logis.fit(model)



cleanEx()
nameEx("logis.noise")
### * logis.noise

flush(stderr()); flush(stdout())

### Name: logis.noise
### Title: Creating a nls model for logistic regression from fitted values
###   of a glm model
### Aliases: logis.noise

### ** Examples

x <- 1:50
y <- c(rep(0,18),sample(0:1,14,replace=TRUE),rep(1,18))
model <- glm(y~x,family=binomial)
y2 <- logis.noise(model)
# Then model2 <- nls(y2~SSlogis(...))



cleanEx()
nameEx("lr.multcomp")
### * lr.multcomp

flush(stderr()); flush(stdout())

### Name: lr.multcomp
### Title: Comparison of several simple linear least rectangles regression
###   lines
### Aliases: lr.multcomp

### ** Examples

x <- rep(1:30,3)
y <- c(1:30+rnorm(30,0,3),seq(10,35,25/29)+rnorm(30,0,3),seq(-27,0,27/29)+rnorm(30,0,3))
fact <- gl(3,30,labels=LETTERS[1:3])
lr.multcomp(y~x|fact)



cleanEx()
nameEx("mat.cont")
### * mat.cont

flush(stderr()); flush(stdout())

### Name: mat.cont
### Title: Matrix of contrasts generation
### Aliases: mat.cont

### ** Examples

# A factor with 5 levels
fact <- gl(5,10,labels=LETTERS[1:5])

# A matrix of contrasts with all possible pairwise comparisons
mat.cont(fact)

# A matrix with only comparisons to level "A"
mat.cont(fact,ref="A")

# A matrix with all pairwise comparisons between "A", "B" and "E"
mat.cont(fact,restrict=c("A","B","E"))

# The same with levels "C" and "D" excluded
mat.cont(fact,restrict=c("A","B","E"),exclude=TRUE)



cleanEx()
nameEx("mod")
### * mod

flush(stderr()); flush(stdout())

### Name: mod
### Title: Mode
### Aliases: mod

### ** Examples

# Continuous variable
x <- rnorm(100)
mod(x)

# Discontinuous variable
y <- rpois(100,2)
mod(y)



cleanEx()
nameEx("mqqnorm")
### * mqqnorm

flush(stderr()); flush(stdout())

### Name: mqqnorm
### Title: Multivariate normality QQ-Plot
### Aliases: mqqnorm

### ** Examples

x <- 1:30+rnorm(30)
y <- 1:30+rnorm(30,1,3)
mqqnorm(rbind(x,y))



cleanEx()
nameEx("pairwise.perm.t.test")
### * pairwise.perm.t.test

flush(stderr()); flush(stdout())

### Name: pairwise.perm.t.test
### Title: Pairwise permutational t tests
### Aliases: pairwise.perm.t.test

### ** Examples

set.seed(1203)
response <- c(rnorm(5),rpois(5,0.5),rnorm(5,2,1))
fact <- gl(3,5,labels=LETTERS[1:3])

# Permutational ANOVA
perm.anova(response~fact)

# Pairwise comparisons
pairwise.perm.t.test(response,fact)



cleanEx()
nameEx("perm.anova")
### * perm.anova

flush(stderr()); flush(stdout())

### Name: perm.anova
### Title: Permutational Analysis of Variance
### Aliases: perm.anova perm.anova.1way perm.anova.2wayA perm.anova.2wayB
###   perm.anova.2wayC perm.anova.2wayD perm.anova.3wayA perm.anova.3wayB

### ** Examples

set.seed(1203)
response <- c(rnorm(12),rpois(12,0.5),rnorm(12,2,1))
fact1 <- gl(3,12,labels=LETTERS[1:3])
fact2 <- gl(3,1,36,labels=letters[1:3])
fact3 <- gl(6,6,labels=letters[1:6])
block <- gl(2,6,36,labels=letters[1:2])

# 1 factor
perm.anova(response~fact1)

# 2 crossed fixed factors without interaction
perm.anova(response~fact1+fact2)

# 2 crossed fixed factors with interaction
perm.anova(response~fact1*fact2)

# 2 nested fixed factors
perm.anova(response~fact1/fact2)

# 2 nested factors, fact2 being random
perm.anova(response~fact1/fact3,nest.f2="random")

# 1 fixed factor and 1 blocking (random) factor
perm.anova(response~fact1|block)

# 2 fixed crossed factors and 1 blocking (random) factor, without interaction
perm.anova(response~fact1+fact2|block)

# 2 fixed crossed factors and 1 blocking (random) factor, with interaction
perm.anova(response~fact1*fact2|block)



cleanEx()
nameEx("perm.bartlett.test")
### * perm.bartlett.test

flush(stderr()); flush(stdout())

### Name: perm.bartlett.test
### Title: Permutational Bartlett's test of homogeneity of variances
### Aliases: perm.bartlett.test

### ** Examples

response <- c(rnorm(12),rpois(12,1),rnorm(12,2,1))
fact <- gl(3,12,labels=LETTERS[1:3])
perm.bartlett.test(response~fact)



cleanEx()
nameEx("perm.cor.test")
### * perm.cor.test

flush(stderr()); flush(stdout())

### Name: perm.cor.test
### Title: Permutational Pearson's correlation test
### Aliases: perm.cor.test

### ** Examples

x <- rnorm(50)
y <- runif(50)
perm.cor.test(x,y)



cleanEx()
nameEx("perm.t.test")
### * perm.t.test

flush(stderr()); flush(stdout())

### Name: perm.t.test
### Title: Permutational Student's t-test
### Aliases: perm.t.test

### ** Examples

response <- c(rnorm(5),rnorm(5,2,1))
fact <- gl(2,5,labels=LETTERS[1:2])

# Unpaired test
perm.t.test(response~fact)

# Paired test
perm.t.test(response~fact,paired=TRUE)



cleanEx()
nameEx("perm.var.test")
### * perm.var.test

flush(stderr()); flush(stdout())

### Name: perm.var.test
### Title: Permutational F test to compare two variances
### Aliases: perm.var.test

### ** Examples

response <- c(rpois(8,1),rpois(8,3))
fact <- gl(2,8,labels=LETTERS[1:2])
perm.var.test(response~fact)



cleanEx()
nameEx("plotsurvivors")
### * plotsurvivors

flush(stderr()); flush(stdout())

### Name: plotsurvivors
### Title: Survivor curve
### Aliases: plotsurvivors

### ** Examples

# 'kidney' dataset in MASS package
data(kidney)
plotsurvivors(kidney$time,kidney$status)



cleanEx()
nameEx("prop.multcomp")
### * prop.multcomp

flush(stderr()); flush(stdout())

### Name: prop.multcomp
### Title: Pairwise comparisons after a test for given proportions
### Aliases: prop.multcomp

### ** Examples

proportions <- sample(c(0,1),60,replace=TRUE)
populations <- sample(LETTERS[1:3],60,replace=TRUE)
tab.cont <- table(populations,proportions)
p.theo <- c(0.2,0.5,0.7)
prop.test(tab.cont,p=p.theo)
prop.multcomp(tab.cont,p=p.theo)



cleanEx()
nameEx("psignif")
### * psignif

flush(stderr()); flush(stdout())

### Name: psignif
### Title: Stars for p-values significance
### Aliases: psignif

### ** Examples

p <- c(0.005,0.02,0.0000001,0.35)
psignif(p)



cleanEx()
nameEx("reg.ci")
### * reg.ci

flush(stderr()); flush(stdout())

### Name: reg.ci
### Title: Confidence intervals of a simple linear regression
### Aliases: reg.ci

### ** Examples

x <- 1:50
y <- 1:50+rnorm(50,0,4)
regression <- lm(y~x)
plot(x,y)
abline(regression)
reg.ci(regression,type="mean",col="red")
reg.ci(regression,type="ind",col="blue")



cleanEx()
nameEx("reg.intcomp")
### * reg.intcomp

flush(stderr()); flush(stdout())

### Name: reg.intcomp
### Title: Common slope and comparison of several simple linear regression
###   intercepts
### Aliases: reg.intcomp

### ** Examples

covariable <- rep(1:30,3)
variable <- c(1:30+rnorm(30,0,3),seq(10,35,25/29)+rnorm(30,0,3),seq(-27,0,27/29)+rnorm(30,0,3))
fact <- gl(3,30,labels=LETTERS[1:3])
reg.intcomp(variable~covariable|fact)



cleanEx()
nameEx("reg.slpcomp")
### * reg.slpcomp

flush(stderr()); flush(stdout())

### Name: reg.slpcomp
### Title: Comparison of several simple linear regression slopes
### Aliases: reg.slpcomp

### ** Examples

covariable <- rep(1:30,3)
variable <- c(seq(1,10,9/29)+rnorm(30,0,3),seq(1,30,1)+rnorm(30,0,3),seq(-1,-80,-79/29)+rnorm(30,0,3))
fact <- gl(3,30,labels=LETTERS[1:3])
reg.slpcomp(variable~covariable|fact)



cleanEx()
nameEx("scat.mix.categorical")
### * scat.mix.categorical

flush(stderr()); flush(stdout())

### Name: scat.mix.categorical
### Title: Representation of qualitative variables in Hill and Smith mix
###   analysis
### Aliases: scat.mix.categorical

### ** Examples

# Fictive dataset
age <- sample(15:60,50,replace=TRUE)
sex <- sample(c("M","F"),50,replace=TRUE)
size <- sample(155:190,50,replace=TRUE)
hair <- sample(c("Fair","Dark","Russet"),50,replace=TRUE)
eyes <- sample(c("Blue","Green","Brown"),50,replace=TRUE)
weight <- sample(50:85,50,replace=TRUE)
hand <- sample(c("Left.handed","Right.handed"),50,replace=TRUE)
tab <- data.frame(age,sex,size,weight,hand,eyes,hair)
amix <- dudi.mix(tab,scannf=FALSE,nf=2)
scat.mix.categorical(amix)



cleanEx()
nameEx("scat.mix.numeric")
### * scat.mix.numeric

flush(stderr()); flush(stdout())

### Name: scat.mix.numeric
### Title: Representation of numeric variables in Hill and Smith mix
###   analysis
### Aliases: scat.mix.numeric

### ** Examples

# Fictive dataset
age <- sample(15:60,50,replace=TRUE)
sex <- sample(c("M","F"),50,replace=TRUE)
size <- sample(155:190,50,replace=TRUE)
hair <- sample(c("Fair","Dark","Russet"),50,replace=TRUE)
eyes <- sample(c("Blue","Green","Brown"),50,replace=TRUE)
weight <- sample(50:85,50,replace=TRUE)
hand <- sample(c("Left.handed","Right.handed"),50,replace=TRUE)
tab <- data.frame(age,sex,size,weight,hand,eyes,hair)
amix <- dudi.mix(tab,scannf=FALSE,nf=2)
scat.mix.numeric(amix)



cleanEx()
nameEx("se")
### * se

flush(stderr()); flush(stdout())

### Name: se
### Title: Standard error of the mean
### Aliases: se

### ** Examples

se(rnorm(30))



cleanEx()
nameEx("seq2")
### * seq2

flush(stderr()); flush(stdout())

### Name: seq2
### Title: Sequence generation
### Aliases: seq2

### ** Examples

seq2(rnorm(30))



cleanEx()
nameEx("spearman.ci")
### * spearman.ci

flush(stderr()); flush(stdout())

### Name: spearman.ci
### Title: Confidence interval of a Spearman's rank correlation coefficient
### Aliases: spearman.ci

### ** Examples

var1 <- sample(1:50,15,replace=TRUE)
var2 <- sample(1:50,15,replace=TRUE)
spearman.ci(var1,var2)



cleanEx()
nameEx("surv.multcomp")
### * surv.multcomp

flush(stderr()); flush(stdout())

### Name: surv.multcomp
### Title: Pairwise comparisons for survival analysis
### Aliases: surv.multcomp

### ** Examples

death <- c(sample(8:15,20,replace=TRUE),sample(12:20,20,replace=TRUE),sample(18:22,20,replace=TRUE))
groups <- gl(3,20,labels=LETTERS[1:3])
model1 <- coxph(Surv(death)~groups)
model1
mat <- matrix(c(1,-1,0,0,1,-1,2,-1,-1),ncol=3,byrow=TRUE,dimnames=list(1:3,levels(groups)))
mat
surv.multcomp(Surv(death)~groups,mat,type="coxph")

model2 <- survreg(Surv(death)~groups,dist="weibull")
model2
surv.multcomp(Surv(death)~groups,mat,type="survreg",distribution="weibull")



cleanEx()
nameEx("wilcox.paired.multcomp")
### * wilcox.paired.multcomp

flush(stderr()); flush(stdout())

### Name: wilcox.paired.multcomp
### Title: Non parametric pairwise comparisons for paired data
### Aliases: wilcox.paired.multcomp

### ** Examples

response <- c(rnorm(10,0,3),rnorm(10,5,3),rnorm(10,8,2))
fact <- gl(3,10,labels=LETTERS[1:3])
block <- gl(10,1,30,labels=letters[1:10])
friedman.test(response~fact|block)
wilcox.paired.multcomp(response~fact|block)



cleanEx()
nameEx("wilcox.signtest")
### * wilcox.signtest

flush(stderr()); flush(stdout())

### Name: wilcox.signtest
### Title: Wilcoxon sign test
### Aliases: wilcox.signtest

### ** Examples

set.seed(1706)
response <- c(rnorm(7,3,1.5),rnorm(7,5.5,2))

# Comparison of 2 samples
fact <- gl(2,7,labels=LETTERS[1:2])
wilcox.signtest(response~fact)

# Comparison to a given value
theo <- 4
wilcox.signtest(response,mu=theo)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
