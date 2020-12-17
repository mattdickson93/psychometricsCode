######Exploratory Factor Analysis and Parallel Analysis######

#Set your working directory
setwd("C:/Users/dd811711/Dropbox/APSY 753--Spring 2017/Data Examples")

#Install and Load Packages
install.packages("psych", dependencies=TRUE)
library(psych)

#Read in data
mydata <- read.csv("EFA Example.csv", header=TRUE)
MS_Scale <- mydata[,c(1:13)]
MTS_Scale <- mydata[,c(11:19)]

#Parallel Analysis for MS
MS_PA <- fa.parallel(MS_Scale, fm="pa", fa="both", main="MS Parallel Analysis", n.iter=1000, SMC=TRUE, show.legend=TRUE)

#Parallel Analysis for MTS
MTS_PA <- fa.parallel(MTS_Scale, fm="pa", fa="both", main="MTS Parallel Analysis", n.iter=1000, SMC=TRUE, show.legend=TRUE)

#EFA for MS
FA_MS1 <- fa(MS_Scale, nfactors=1, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)
FA_MS2 <- fa(MS_Scale, nfactors=2, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)
FA_MS3 <- fa(MS_Scale, nfactors=3, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)
FA_MS4 <- fa(MS_Scale, nfactors=4, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)

#Results
print(FA_MS3, sort=TRUE)

#EFA for MTS
FA_MTS1 <- fa(MTS_Scale, nfactors=1, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)
FA_MTS2 <- fa(MTS_Scale, nfactors=2, rotate="oblimin", SMC=TRUE, max.iter=1000, fm="pa", alpha=.05)

#Results
print(FA_MTS1, sort=TRUE)



