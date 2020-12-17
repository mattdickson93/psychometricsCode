#####POLYTOMOUS IRT IN R#####

#Set working director
setwd("C:/Users/dd811711/Dropbox/APSY 753--Spring 2017/Data Examples")

##To Read Data
Con <- read.csv("Cons Data.csv", header=TRUE)
Con <- Con[,-1]

##Install and Load LTM
install.packages("ltm", dependencies=TRUE)
library(ltm)

##Check Dimensionality of the Data
library(psych)
fa.parallel(Con,fa="both")

##To Run/Plot GPCM
CONGPCM <- gpcm(Con, constraint = c("gpcm"), IRT.param = TRUE, control=list(iter.qN = 1000))
GPCMICC <- plot(CONGPCM, type="ICC", items=NULL, zrange=c(-4,4), annot=TRUE)
GPCMIIC <- plot(CONGPCM, type="IIC", items=NULL, zrange=c(-4,4), annot=TRUE)
GPCMTIC <- plot(CONGPCM, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)

##To Run/Plot GRM
CONGRM <- grm(Con, constrained = FALSE, IRT.param=TRUE, control=list(iter.qN = 1000))
GRMICC <- plot(CONGRM, type="ICC", items=NULL, zrange=c(-4,4), annot=TRUE)
GRMIIC <- plot(CONGRM, type="IIC", items=NULL, zrange=c(-4,4), annot=TRUE)
GRMTIC <- plot(CONGRM, type="IIC", items=0, zrange=c(-4,4), annot=TRUE)

