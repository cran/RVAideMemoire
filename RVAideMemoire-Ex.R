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
samp<-sample(1:50,10,replace=TRUE)
bootstrap(samp,function(x,i) mean(x[i]))

# Confidence interval of the standard error of the mean
bootstrap(samp,function(x,i) sd(x[i])/sqrt(length(x[i])))



cleanEx()
nameEx("chisq.exp")
### * chisq.exp

flush(stderr()); flush(stdout())

### Name: chisq.exp
### Title: Expected counts for comparison of proportions to given values
### Aliases: chisq.exp

### ** Examples

proportions<-sample(c(0,1),60,replace=TRUE)
populations<-sample(LETTERS[1:3],60,replace=TRUE)
tab.cont<-table(populations,proportions)
p.theo<-c(0.2,0.5,0.7)
chisq.exp(tab.cont,p=p.theo)



cleanEx()
nameEx("chisq.multcomp")
### * chisq.multcomp

flush(stderr()); flush(stdout())

### Name: chisq.multcomp
### Title: Pairwise comparisons after chi-squared goodness-of-fit test
### Aliases: chisq.multcomp

### ** Examples

counts<-c(5,15,23,8,14)
chisq.test(counts)
chisq.multcomp(counts)



cleanEx()
nameEx("chisq.theo.multcomp")
### * chisq.theo.multcomp

flush(stderr()); flush(stdout())

### Name: chisq.theo.multcomp
### Title: Pairwise comparisons after chi-squared test for given
###   probabilities
### Aliases: chisq.theo.multcomp

### ** Examples

counts<-c(5,15,23,8,14)
p.theo<-c(0.1,0.4,0.3,0.15,0.05)
chisq.test(counts,p=p.theo)
chisq.theo.multcomp(counts,p=p.theo)



cleanEx()
nameEx("cochran.qtest")
### * cochran.qtest

flush(stderr()); flush(stdout())

### Name: cochran.qtest
### Title: Cochran Q test
### Aliases: cochran.qtest

### ** Examples

response<-c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1)
factor<-factor(rep(LETTERS[1:3],10))
block<-factor(rep(letters[1:10],each=3))
cochran.qtest(response~factor|block)



cleanEx()
nameEx("cor.2comp")
### * cor.2comp

flush(stderr()); flush(stdout())

### Name: cor.2comp
### Title: Comparison of 2 Pearson's linear correlation coefficients
### Aliases: cor.2comp

### ** Examples

cor1.var1<-1:30+rnorm(30,0,2)
cor1.var2<-1:30+rnorm(30,0,3)
cor2.var1<-(-1):-30+rnorm(30,0,2)
cor2.var2<-(-1):-30+rnorm(30,0,3)
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

var1<-1:30+rnorm(30,0,4)
var2<-1:30+rnorm(30,0,4)
cor.conf(var1,var2,theo=0.5)



cleanEx()
nameEx("cor.multcomp")
### * cor.multcomp

flush(stderr()); flush(stdout())

### Name: cor.multcomp
### Title: Comparison of Pearson's linear correlation coefficients
### Aliases: cor.multcomp

### ** Examples

var1<-c(1:15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
var2<-c(-1:-15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
factor<-factor(rep(LETTERS[1:3],each=15))
cor.multcomp(var1,var2,factor)

var3<-c(1:15+rnorm(15,0,1),1:15+rnorm(15,0,3),1:15+rnorm(15,0,2))
cor.multcomp(var1,var3,factor)



cleanEx()
nameEx("cox.resid")
### * cox.resid

flush(stderr()); flush(stdout())

### Name: cox.resid
### Title: Martingale residuals of Cox model
### Aliases: cox.resid

### ** Examples

# 'kidney' dataset in MASS package
data(kidney)
model<-coxph(Surv(time,status)~age+frail,data=kidney)
cox.resid(model,list(age=kidney$age,frail=kidney$frail))



cleanEx()
nameEx("cramer.coeff")
### * cramer.coeff

flush(stderr()); flush(stdout())

### Name: cramer.coeff
### Title: Cramer's association coefficient
### Aliases: cramer.coeff

### ** Examples

var1<-sample(LETTERS[1:3],15,replace=TRUE)
var2<-sample(letters[1:3],15,replace=TRUE)
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
nameEx("fisher.medtest")
### * fisher.medtest

flush(stderr()); flush(stdout())

### Name: fisher.medtest
### Title: Comparison of 2 medians by Fisher's exact test
### Aliases: fisher.medtest

### ** Examples

x<-rnorm(4,3,1.5)
y<-rnorm(4,5.5,2)
fisher.medtest(x,y)



cleanEx()
nameEx("fisher.multcomp")
### * fisher.multcomp

flush(stderr()); flush(stdout())

### Name: fisher.multcomp
### Title: Pairwise comparisons after a test for independence of 2
###   categorical variables
### Aliases: fisher.multcomp

### ** Examples

tab.cont<-as.table(matrix(c(25,10,12,6,15,14,9,16,9),ncol=3,dimnames=list(c("fair","dark","russet"),c("blue","brown","green"))))
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

x<-1:30
y<-1:30+rnorm(30,0,4)
model1<-lm(y~x)
model2<-least.rect(y~x)
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

x<-1:30+rnorm(30,0,3)
y<-1:30+rnorm(30,0,3)
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

x<-1:50
y<-c(rep(0,18),sample(0:1,14,replace=TRUE),rep(1,18))
model<-glm(y~x,family=binomial)
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

x<-1:50
y<-c(rep(0,18),sample(0:1,14,replace=TRUE),rep(1,18))
model<-glm(y~x,family=binomial)
y2<-logis.noise(model)
# Then model2<-nls(y2~SSlogis(...))



cleanEx()
nameEx("lr.multcomp")
### * lr.multcomp

flush(stderr()); flush(stdout())

### Name: lr.multcomp
### Title: Comparison of simple linear least rectangles regression lines
### Aliases: lr.multcomp

### ** Examples

x<-rep(1:30,3)
y<-c(1:30+rnorm(30,0,3),seq(10,35,25/29)+rnorm(30,0,3),seq(-27,0,27/29)+rnorm(30,0,3))
factor<-as.factor(rep(LETTERS[1:3],each=30))
lr.multcomp(y~x|factor)



cleanEx()
nameEx("mod")
### * mod

flush(stderr()); flush(stdout())

### Name: mod
### Title: Mode
### Aliases: mod

### ** Examples

mod(rnorm(100))



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

proportions<-sample(c(0,1),60,replace=TRUE)
populations<-sample(LETTERS[1:3],60,replace=TRUE)
tab.cont<-table(populations,proportions)
p.theo<-c(0.2,0.5,0.7)
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

p<-c(0.005,0.02,0.0000001,0.35)
psignif(p)



cleanEx()
nameEx("reg.ci")
### * reg.ci

flush(stderr()); flush(stdout())

### Name: reg.ci
### Title: Confidence intervals of a simple linear regression
### Aliases: reg.ci

### ** Examples

x<-1:50
y<-1:50+rnorm(50,0,4)
regression<-lm(y~x)
plot(x,y)
abline(regression)
reg.ci(regression,type="mean",col="red")
reg.ci(regression,type="ind",col="blue")



cleanEx()
nameEx("reg.intcomp")
### * reg.intcomp

flush(stderr()); flush(stdout())

### Name: reg.intcomp
### Title: Common slope and comparison of simple linear regression
###   intercepts
### Aliases: reg.intcomp

### ** Examples

covariable<-rep(1:30,3)
variable<-c(1:30+rnorm(30,0,3),seq(10,35,25/29)+rnorm(30,0,3),seq(-27,0,27/29)+rnorm(30,0,3))
factor<-as.factor(rep(LETTERS[1:3],each=30))
reg.intcomp(variable~covariable|factor)



cleanEx()
nameEx("reg.slpcomp")
### * reg.slpcomp

flush(stderr()); flush(stdout())

### Name: reg.slpcomp
### Title: Comparison of simple linear regression slopes
### Aliases: reg.slpcomp

### ** Examples

covariable<-rep(1:30,3)
variable<-c(seq(1,10,9/29)+rnorm(30,0,3),seq(1,30,1)+rnorm(30,0,3),seq(-1,-80,-79/29)+rnorm(30,0,3))
factor<-factor(rep(LETTERS[1:3],each=30))
reg.slpcomp(variable~covariable|factor)



cleanEx()
nameEx("scat.mix.categorical")
### * scat.mix.categorical

flush(stderr()); flush(stdout())

### Name: scat.mix.categorical
### Title: Representation of qualitative variables in Hill and Smith mix
###   analysis
### Aliases: scat.mix.categorical

### ** Examples

# Imaginary dataset
age<-sample(15:60,50,replace=TRUE)
sex<-sample(c("M","F"),50,replace=TRUE)
size<-sample(155:190,50,replace=TRUE)
hair<-sample(c("Fair","Dark","Russet"),50,replace=TRUE)
eyes<-sample(c("Blue","Green","Brown"),50,replace=TRUE)
weight<-sample(50:85,50,replace=TRUE)
hand<-sample(c("Left.handed","Right.handed"),50,replace=TRUE)
tab<-data.frame(age,sex,size,weight,hand,eyes,hair)
amix<-dudi.mix(tab,scannf=FALSE,nf=2)
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

# Imaginary dataset
age<-sample(15:60,50,replace=TRUE)
sex<-sample(c("M","F"),50,replace=TRUE)
size<-sample(155:190,50,replace=TRUE)
hair<-sample(c("Fair","Dark","Russet"),50,replace=TRUE)
eyes<-sample(c("Blue","Green","Brown"),50,replace=TRUE)
weight<-sample(50:85,50,replace=TRUE)
hand<-sample(c("Left.handed","Right.handed"),50,replace=TRUE)
tab<-data.frame(age,sex,size,weight,hand,eyes,hair)
amix<-dudi.mix(tab,scannf=FALSE,nf=2)
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

var1<-sample(1:50,15,replace=TRUE)
var2<-sample(1:50,15,replace=TRUE)
spearman.ci(var1,var2)



cleanEx()
nameEx("surv.multcomp")
### * surv.multcomp

flush(stderr()); flush(stdout())

### Name: surv.multcomp
### Title: Pairwise comparisons for survival analysis
### Aliases: surv.multcomp

### ** Examples

death<-c(sample(8:15,20,replace=TRUE),sample(12:20,20,replace=TRUE),sample(18:22,20,replace=TRUE))
groups<-factor(rep(LETTERS[1:3],each=20))
model1<-coxph(Surv(death)~groups)
model1
mat<-matrix(c(1,-1,0,0,1,-1,2,-1,-1),ncol=3,byrow=TRUE,dimnames=list(1:3,levels(groups)))
surv.multcomp(Surv(death)~groups,mat,type="coxph")

model2<-survreg(Surv(death)~groups,dist="weibull")
model2
surv.multcomp(Surv(death)~groups,mat,type="survreg",distribution="weibull")



cleanEx()
nameEx("wilcox.paired.multcomp")
### * wilcox.paired.multcomp

flush(stderr()); flush(stdout())

### Name: wilcox.paired.multcomp
### Title: Pairwise Wilcoxon signed rank sum test
### Aliases: wilcox.paired.multcomp

### ** Examples

response<-c(rnorm(10,0,3),rnorm(10,5,3),rnorm(10,8,2))
factor<-factor(rep(LETTERS[1:3],each=10))
block<-factor(rep(letters[1:10],3))
friedman.test(response~factor|block)
wilcox.paired.multcomp(response~factor|block)



cleanEx()
nameEx("wilcox.signtest")
### * wilcox.signtest

flush(stderr()); flush(stdout())

### Name: wilcox.signtest
### Title: Wilcoxon sign test
### Aliases: wilcox.signtest

### ** Examples

x<-rnorm(7,3,1.5)
y<-rnorm(7,5.5,2)
wilcox.signtest(x,y)

theo=4
wilcox.signtest(x,mu=theo)



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
