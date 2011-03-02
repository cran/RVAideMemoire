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

# Intervalle de confiance a 95 pc d'une moyenne sur petit echantillon
ech<-sample(1:50,10,replace=TRUE)
bootstrap(ech,function(x,i) mean(x[i]))

# La meme chose avec un intervalle de confiance a 99 pc
bootstrap(ech,function(x,i) mean(x[i]),conf.level=0.99)

# Un calcul (un peu) plux complexe, l'intervalle de confiance de l'erreur standard
# de la moyenne
bootstrap(ech,function(x,i) sd(x[i])/sqrt(length(x[i])))



cleanEx()
nameEx("cook.dist")
### * cook.dist

flush(stderr()); flush(stdout())

### Name: cook.dist
### Title: Graphe des distances de Cook individuelles dans une regression
###   lineaire simple au sens des moindres carres
### Aliases: cook.dist

### ** Examples

# Graphe par defaut
x<-1:50
y<-1:50+rnorm(50,0,4)
regression<-lm(y~x)
cook.dist(regression)

# Avertissement quand la distance vaut 80 pc de la distance theorique
cook.dist(regression,avert=0.8)



cleanEx()
nameEx("cor.2comp")
### * cor.2comp

flush(stderr()); flush(stdout())

### Name: cor.2comp
### Title: Comparaison de 2 coefficients de correlation lineaire de Pearson
### Aliases: cor.2comp

### ** Examples

# Test par defaut
cor1.var1<-1:30+rnorm(30,0,2)
cor1.var2<-1:30+rnorm(30,0,3)
cor2.var1<--1:-30+rnorm(30,0,2)
cor2.var2<--1:-30+rnorm(30,0,3)
cor.2comp(cor1.var1,cor1.var2,cor2.var1,cor2.var2)

# Seuil de significativite a 3 pc et conformite du coefficient de correlation commun
# avec la valeur 0.5
 cor.2comp(cor1.var1,cor1.var2,cor2.var1,cor2.var2,alpha=0.03,theo=0.5)



cleanEx()
nameEx("cor.conf")
### * cor.conf

flush(stderr()); flush(stdout())

### Name: cor.conf
### Title: Conformite d'un coefficient de correlation lineaire de Pearson
###   avec une valeur theorique
### Aliases: cor.conf

### ** Examples

var1<-1:30+rnorm(30,0,4)
var2<-1:30+rnorm(30,0,4)
cor.conf(var1,var2,theo=0.8)



cleanEx()
nameEx("cor.multcomp")
### * cor.multcomp

flush(stderr()); flush(stdout())

### Name: cor.multcomp
### Title: Comparaison de plusieurs coefficients de correlation lineaire de
###   Pearson
### Aliases: cor.multcomp

### ** Examples

# Test par defaut
var1<-c(1:15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
var2<-c(-1:-15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
facteur<-as.factor(rep(LETTERS[1:3],each=15))
cor.multcomp(var1,var2,facteur)

# Correction des p-values avec la methode de Bonferroni
cor.multcomp(var1,var2,facteur,p.method="bonferroni")



cleanEx()
nameEx("cramer.cor")
### * cramer.cor

flush(stderr()); flush(stdout())

### Name: cramer.cor
### Title: Coefficient d'association de Cramer
### Aliases: cramer.cor

### ** Examples

# Calcul par defaut
var1<-sample(LETTERS[1:3],15,replace=TRUE)
var2<-sample(letters[1:3],15,replace=TRUE)
cramer.cor(var1,var2)

# Bootstrap a 100 repetitions et intervalle de confiance a 99 pc
cramer.cor(var1,var2,rep=100,conf.level=0.99)



cleanEx()
nameEx("cv")
### * cv

flush(stderr()); flush(stdout())

### Name: cv
### Title: Coefficient de variation
### Aliases: cv

### ** Examples

# Coefficient par defaut
serie<-sample(1:30,20)
cv(serie)

# Coefficient en proportion
cv(serie,pc=FALSE)



cleanEx()
nameEx("eff.theo")
### * eff.theo

flush(stderr()); flush(stdout())

### Name: eff.theo
### Title: Conformite de plusieurs proportions avec des valeurs theoriques
###   - effectifs theoriques
### Aliases: eff.theo

### ** Examples

# Pas de graphe trace (fonctionnement par defaut)
pop<-sample(LETTERS[1:3],60,replace=TRUE)
prop<-sample(c(0,1),60,replace=TRUE)
tab.cont<-table(pop,prop)
prop.theo<-c(0.2,0.5,0.7)
eff.theo(tab.cont,p.theo=prop.theo)

# Avec une representation graphique du tableau des effectifs theoriques
eff.theo(tab.cont,p.theo=prop.theo,graphe=TRUE)



cleanEx()
nameEx("least.rect")
### * least.rect

flush(stderr()); flush(stdout())

### Name: least.rect
### Title: Regression lineaire simple au sens des moindres rectangles
### Aliases: least.rect

### ** Examples

# Resume par defaut
x<-1:30+rnorm(30,0,3)
y<-1:30+rnorm(30,0,3)
least.rect(x,y)

# Conformite du coefficient directeur avec la valeur 0.5
# et intervalles de confiance a 99 pc
least.rect(x,y,conf.level=0.99,theo=0.5)



cleanEx()
nameEx("logis.fit")
### * logis.fit

flush(stderr()); flush(stdout())

### Name: logis.fit
### Title: Ajustement graphique d'une regression logistique binaire simple
###   aux donnees intitiales
### Aliases: logis.fit

### ** Examples

x<-1:50
y<-c(rep(0,15),sample(0:1,20,replace=TRUE),rep(1,15))
modele<-glm(y~x,family=binomial)
plot(x,y)
lines(x,modele$fitted.values)
logis.fit(modele)



cleanEx()
nameEx("mod")
### * mod

flush(stderr()); flush(stdout())

### Name: mod
### Title: Mode
### Aliases: mod

### ** Examples

serie<-sample(1:50,30)
mod(serie)



cleanEx()
nameEx("rect.line")
### * rect.line

flush(stderr()); flush(stdout())

### Name: rect.line
### Title: Droite de regression lineaire simple au sens des moindres
###   rectangles
### Aliases: rect.line

### ** Examples

x<-1:30+rnorm(30,0,3)
y<-1:30+rnorm(30,0,3)
abline(rect.line(x,y))



cleanEx()
nameEx("reg.ci")
### * reg.ci

flush(stderr()); flush(stdout())

### Name: reg.ci
### Title: Intervalles de confiance d'une regression lineaire simple au
###   sens des moindres carres
### Aliases: reg.ci

### ** Examples

# Intervalle de confiance des moyennes conditionnelles
x<-1:50
y<-1:50+rnorm(50,0,4)
regression<-lm(y~x)
plot(x,y)
abline(regression)
reg.ci(regression,type="moy",col="red")

# Intervalle de confiance a 99 pc des valeurs individuelles
reg.ci(regression,conf.level=0.99,type="ind",col="blue",lty=2)



cleanEx()
nameEx("reg.dircomp")
### * reg.dircomp

flush(stderr()); flush(stdout())

### Name: reg.dircomp
### Title: Comparaison des coefficients directeurs de plusieurs droites de
###   regression lineaire simple au sens des moindres carres
### Aliases: reg.dircomp

### ** Examples

covar<-rep(1:30,3)
var<-c(seq(1,10,9/29)+rnorm(30,0,3),seq(1,30,1)+rnorm(30,0,3),seq(-1,-80,-79/29)+rnorm(30,0,3))
facteur<-as.factor(rep(LETTERS[1:3],each=30))
reg.dircomp(var,covar,facteur)



cleanEx()
nameEx("reg.ordcomp")
### * reg.ordcomp

flush(stderr()); flush(stdout())

### Name: reg.ordcomp
### Title: Comparaison des ordonnees a l'origine de plusieurs droites de
###   regression lineaire simple au sens des moindres carres et coefficient
###   directeur commun
### Aliases: reg.ordcomp

### ** Examples

# Tests par defaut
covar<-rep(1:30,3)
var<-c(1:30+rnorm(30,0,3),seq(10,35,25/29)+rnorm(30,0,3),seq(-27,0,27/29)+rnorm(30,0,3))
facteur<-as.factor(rep(LETTERS[1:3],each=30))
reg.ordcomp(var,covar,facteur)

# Intervalles de confiance a 99 pc, ordonnees a l'origine theoriques non nulles
# et correction des p-values par la methode de Bonferroni
reg.ordcomp(var,covar,facteur,conf.level=0.99,theo=c(1,8,-20),p.method="bonferroni")



cleanEx()
nameEx("scat.mix.categorical")
### * scat.mix.categorical

flush(stderr()); flush(stdout())

### Name: scat.mix.categorical
### Title: Representation des variables qualitatives en analyse mixte de
###   Hill et Smith
### Aliases: scat.mix.categorical

### ** Examples

# Jeu de donnees bidon
age<-sample(15:60,50,replace=TRUE)
sexe<-sample(c("M","F"),50,replace=TRUE)
taille<-sample(155:190,50,replace=TRUE)
cheveux<-sample(c("Blond","Brun","Roux"),50,replace=TRUE)
yeux<-sample(c("Bleu","Vert","Marron"),50,replace=TRUE)
poids<-sample(50:85,50,replace=TRUE)
cote<-sample(c("Gaucher","Droitier"),50,replace=TRUE)
tab<-data.frame(age,sexe,taille,poids,cote,yeux,cheveux)
amix<-dudi.mix(tab,scannf=FALSE,nf=2)
scat.mix.categorical(amix)



cleanEx()
nameEx("scat.mix.numeric")
### * scat.mix.numeric

flush(stderr()); flush(stdout())

### Name: scat.mix.numeric
### Title: Representation des variables numeriques en analyse mixte de Hill
###   et Smith
### Aliases: scat.mix.numeric

### ** Examples

# Jeu de donnees bidon
age<-sample(15:60,50,replace=TRUE)
sexe<-sample(c("M","F"),50,replace=TRUE)
taille<-sample(155:190,50,replace=TRUE)
cheveux<-sample(c("Blond","Brun","Roux"),50,replace=TRUE)
yeux<-sample(c("Bleu","Vert","Marron"),50,replace=TRUE)
poids<-sample(50:85,50,replace=TRUE)
cote<-sample(c("Gaucher","Droitier"),50,replace=TRUE)
tab<-data.frame(age,sexe,taille,poids,cote,yeux,cheveux)
amix<-dudi.mix(tab,scannf=FALSE,nf=2)
scat.mix.numeric(amix)



cleanEx()
nameEx("se")
### * se

flush(stderr()); flush(stdout())

### Name: se
### Title: Erreur standard de la moyenne
### Aliases: se

### ** Examples

serie<-sample(1:50,30)
se(serie)



cleanEx()
nameEx("spearman.ci")
### * spearman.ci

flush(stderr()); flush(stdout())

### Name: spearman.ci
### Title: Intervalle de confiance d'un coefficient de correlation lineaire
###   de Spearman
### Aliases: spearman.ci

### ** Examples

# Intervalle de confiance par defaut
var1<-sample(1:50,15,replace=TRUE)
var2<-sample(1:50,15,replace=TRUE)
spearman.ci(var1,var2)

# Bootstrap a 100 repetitions et intervalle de confiance a 99 pc
spearman.ci(var1,var2,rep=100,conf.level=0.99)



cleanEx()
nameEx("tend.curve")
### * tend.curve

flush(stderr()); flush(stdout())

### Name: tend.curve
### Title: Courbe de tendance d'un nuage de points
### Aliases: tend.curve

### ** Examples

x<-1:30
y<-rexp(30,3)
plot(x,y)
tend.curve(x,y)



cleanEx()
nameEx("val.resid")
### * val.resid

flush(stderr()); flush(stdout())

### Name: val.resid
### Title: Analyse graphique des residus d'un modele lineaire ou lineaire
###   generalise
### Aliases: val.resid

### ** Examples

# Regression
x<-1:30
y<-1:30+rnorm(30,0,2)
modele<-lm(y~x)
val.resid(modele,reg=TRUE)

# Analyse de variance
f<-factor(rep(LETTERS[1:3],each=10))
modele2<-lm(y~f)
val.resid(modele2,reg=FALSE,fact="f")



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
