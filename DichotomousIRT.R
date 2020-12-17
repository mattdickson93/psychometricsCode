###Dichotomous IRT Examples###

setwd("C:/Users/dd811711/Dropbox/APSY 753--Spring 2017/Data Examples")

#Install and Load packages
install.packages("ltm", dependencies=TRUE)
library(ltm)

#Read in data
Crowne <- read.csv("Dichotomous IRT Example.csv", header=TRUE)

###Rasch Model###
#Check for unidimensionality
unidimTest(rasch(Crowne))

#Run the Rasch Model
CRasch <- rasch(Crowne, constraint=cbind(ncol(Crowne)+1,1), IRT.param=TRUE, na.action=NULL)
summary(CRasch)

#Assess Item Fit
RaschIFit <- item.fit(CRasch, G=10, FUN=mean, simulate.p.value=TRUE, B=10)
RaschPFit <- person.fit(CRasch, alternative = 'two.sided', resp.patterns=NULL, simulate.p.value=TRUE, B=10)

#Plot ICC/IRF
RaschICC <- plot(CRasch, type="ICC", items=NULL, zrange=c(-4,4), annot=TRUE)
RaschICC <- plot(CRasch, type="ICC", items=1:3, zrange=c(-4,4), annot=TRUE)

#Plot IIC/IIF
RaschIIC <- plot(CRasch, type="IIC", items=NULL, zrange=c(-4,4), annot=TRUE)
RaschIIC <- plot(CRasch, type="IIC", items=1:3, zrange=c(-4,4), annot=TRUE)

#Plot TIC/TIF
RaschTIF <- plot(CRasch, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)


###2PL Model###
#Check for unidimensionality
unidimTest(ltm(Crowne~z1))

#Run the Model
C2PL <- ltm(Crowne~z1, constraint=NULL, IRT.param=TRUE, na.action=NULL)
summary(C2PL)

#Assess Item Fit
IFit2PL <- item.fit(C2PL, G=10, FUN=mean, simulate.p.value=TRUE, B=10)
PFit2PL <- person.fit(C2PL, alternative = 'two.sided', resp.patterns=NULL, simulate.p.value=TRUE, B=10)

#Plot ICC/IRF
ICC2PL <- plot(C2PL, type="ICC", items=NULL, zrange=c(-4,4), annot=TRUE)
ICC2PL <- plot(C2PL, type="ICC", items=1:3, zrange=c(-4,4), annot=TRUE)

#Plot IIC/IIF
IIC2PL <- plot(C2PL, type="IIC", items=NULL, zrange=c(-4,4), annot=TRUE)
IIC2PL <- plot(C2PL, type="IIC", items=1:3, zrange=c(-4,4), annot=TRUE)

#Plot TIC/TIF
TIF2PL <- plot(C2PL, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)


###3PL Model###
#Check for unidimensionality
unidimTest(tpm(Crowne))

#Run the Rasch Model
C3PL <- tpm(Crowne, type="latent.trait", max.guessing = 1, constraint=NULL, IRT.param=TRUE, na.action=NULL)
C3PL

#Assess Item Fit
IFit3PL <- item.fit(C3PL, G=10, FUN=mean, simulate.p.value=TRUE, B=10)
PFit3PL <- person.fit(C3PL, alternative = 'two.sided', resp.patterns=NULL, simulate.p.value=TRUE, B=10)

#Plot ICC/IRF
ICC3PL <- plot(C3PL, type="ICC", items=NULL, zrange=c(-4,4), annot=TRUE)
ICC3PL <- plot(C3PL, type="ICC", items=1:3, zrange=c(-4,4), annot=TRUE)

#Plot IIC/IIF
IIC3PL <- plot(C3PL, type="IIC", items=NULL, zrange=c(-4,4), annot=TRUE)
IIC3PL <- plot(C3PL, type="IIC", items=1:3, zrange=c(-4,4), annot=TRUE)

#Plot TIC/TIF
TIF3PL <- plot(C3PL, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)

