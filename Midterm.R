#####MIDTERM MATT DICKSON#######

####Dichotomous IRT
#Load Packages
library(ltm)

#Read in data
ItemsD <- read.csv("Dichotomous Items.csv", header=TRUE)

###Rasch - 1PL
#Unidimensionality Test
unidimTest(rasch(ItemsD))

#Run the Rasch Model
DRasch <- rasch(ItemsD, constraint=cbind(ncol(Crowne)+1,1), IRT.param=TRUE, na.action=NULL)
summary(DRasch)

#Assess Item Fit
RaschIFit <- item.fit(DRasch, G=10, FUN=mean, simulate.p.value=TRUE, B=10)
RaschPFit <- person.fit(DRasch, alternative = 'two.sided', resp.patterns=NULL, simulate.p.value=TRUE, B=10)

#Plot Rasch Model
RaschICC <- plot(DRasch, type="ICC", items=NULL, zrange=c(-4,4), annot=TRUE)
RaschIIC <- plot(DRasch, type="IIC", items=NULL, zrange=c(-4,4), annot=TRUE)
RaschTIF <- plot(DRasch, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)

###2PL###
#Unidimensionality Test
unidimTest(ltm(ItemsD~z1))

#Run the Model
D2PL <- ltm(ItemsD~z1, constraint=NULL, IRT.param=TRUE, na.action=NULL)
summary(D2PL)

#Assess Fit
IFit2PL <- item.fit(D2PL, G=10, FUN=mean, simulate.p.value=TRUE, B=10)
PFit2PL <- person.fit(D2PL, alternative = 'two.sided', resp.patterns=NULL, simulate.p.value=TRUE, B=10)

#Plot 2PL
ICC2PL <- plot(D2PL, type="ICC", items=NULL, zrange=c(-4,4), annot=TRUE)
IIC2PL <- plot(D2PL, type="IIC", items=NULL, zrange=c(-4,4), annot=TRUE)
TIF2PL <- plot(D2PL, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)

###3PL###

#Unidimensionality Test
unidimTest(tpm(ItemsD))

#Run the 3PL Model
D3PL <- tpm(ItemsD, type="latent.trait", max.guessing = 1, constraint=NULL, IRT.param=TRUE, na.action=NULL)
D3PL

#Assess Fit
IFit3PL <- item.fit(D3PL, G=10, FUN=mean, simulate.p.value=TRUE, B=10)
PFit3PL <- person.fit(D3PL, alternative = 'two.sided', resp.patterns=NULL, simulate.p.value=TRUE, B=10)

#Plot 3PL
ICC3PL <- plot(D3PL, type="ICC", items=NULL, zrange=c(-4,4), annot=TRUE)
IIC3PL <- plot(D3PL, type="IIC", items=NULL, zrange=c(-4,4), annot=TRUE)
TIF3PL <- plot(D3PL, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)

###Examining Item Removal###
#Removal of Item 3
newdata1 <- ItemsD [c(1,2,4,5)]
D2PLA <- ltm(newdata1~z1, constraint=NULL, IRT.param=TRUE, na.action=NULL)
TIF2PL <- plot(D2PLA, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)

#Removal of Item 1
newdata2 <- ItemsD [c(2:5)]
D2PLB <- ltm(newdata2~z1, constraint=NULL, IRT.param=TRUE, na.action=NULL)
TIF2PL <- plot(D2PLB, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)

#Removal of Items 1 AND 3
newdata3 <- ItemsD [c(2,4,5)]
D2PLC <- ltm(newdata3~z1, constraint=NULL, IRT.param=TRUE, na.action=NULL)
TIF2PL <- plot(D2PLC, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)


####Alpha Analysis####
#Load Packages
library(psych)

#Read in Data 
ItemsP <- read.csv("Polytomous Items.csv", header=TRUE)

#Run Alpha
alpha(ItemsP, title = "Polytomous Items", na.rm=TRUE)

####Parallel Analysis and Exploratory Factor Analysis####

#Parallel Analysis of Polytomous Items
IP_PA <- fa.parallel(ItemsP, fm="pa", fa="both", main="IP Parallel Analysis", n.iter=1000, SMC=TRUE, show.legend=TRUE)

#EFA for Polytomous Items
FA_IP1 <- fa(ItemsP, nfactors=1, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)
FA_IP2 <- fa(ItemsP, nfactors=2, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)
FA_IP3 <- fa(ItemsP, nfactors=3, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)
FA_IP4 <- fa(ItemsP, nfactors=4, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)

#Examine Factor Models
print(FA_IP1)
print(FA_IP2)
print(FA_IP3)

####Confirmatory Factor Analysis####
#Load Packages
library(lavaan)

#Three Factor Solution
P3Factor = 'F1 =~ Item2 + Item6 + Item8
F2 =~ Item1 + Item4 + Item10
F3 =~ Item3 + Item5 + Item7 + Item9
F1~~F2
F1~~F3
F2~~F3'

CFAIP <- cfa(IP3Factor, data=ItemsP, std.lv=TRUE, orthogonal=FALSE)
summary(CFAIP, standardized=TRUE, fit.measure=TRUE)













