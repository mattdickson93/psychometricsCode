###MACS Analysis in R###
###Set of consecutively more strict models###

#Set working directory
setwd("C:/Users/Dev Dalal/Dropbox/APSY 753--Spring 2017/Data Examples")

#Install and Load Packages
install.packages("lavaan", dependencies=TRUE)


library(lavaan)


##Load Data
MACS <- read.csv("MACS Data.csv", header=TRUE)


#Does the single factor structure each country individually in the first place?#
#############MTS
###Individual Country Model
#Constructing USA and nonUSA groups#
USA <- subset(MACS, Country==0)
NonUSA <- subset(MACS, Country==1)

Full <- 'MTS =~ MTS1 + MTS2 + MTS3 + MTS4 + MTS5 + MTS6 + MTS7 + MTS8 + MTS9'
Fit.USA <- cfa(Full, data=USA, meanstructure=TRUE, std.lv=TRUE)
Fit.NonUSA <- cfa(Full, data=NonUSA, meanstructure=TRUE, std.lv=TRUE)
summary(Fit.USA, fit.measures=TRUE, standardized=TRUE, ci=TRUE)
summary(Fit.NonUSA, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

#looking to see if the PATTERNS of loadings are the same across groups#
#Values do not need to be the same#
#Between Groups analysis according to "Country"#
###Configural Model###
Config <- 'MTS =~ MTS1 + MTS2 + MTS3 + MTS4 + MTS5 + MTS6 + MTS7 + MTS8 + MTS9'
Fit.Config <- cfa(Config, data=MACS, group="Country", meanstructure=TRUE, std.lv=TRUE)
summary(Fit.Config, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

###Metric Model###
Metric <- 'MTS =~ MTS1 + MTS2 + MTS3 + MTS4 + MTS5 + MTS6 + MTS7 + MTS8 + MTS9'
Fit.Metric <- cfa(Metric, data=MACS, group="Country", meanstructure=TRUE, std.lv=TRUE, group.equal=c("loadings"))
summary(Fit.Metric, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

#Does Configural fit singificantly better than Metric?
anova(Fit.Metric, Fit.Config)
#No!#

###Scalar Model
Scalar <- 'MTS =~ MTS1 + MTS2 + MTS3 + MTS4 + MTS5 + MTS6 + MTS7 + MTS8 + MTS9'
Fit.Scalar <- cfa(Scalar, data=MACS, group="Country", meanstructure=TRUE, std.lv=TRUE, group.equal=c("loadings","intercepts"))
summary(Fit.Scalar, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

#Does Metric fit significantly better than Scalar?
anova(Fit.Scalar, Fit.Metric)

###Strict Factorial Model
Strict <- 'MTS =~ MTS1 + MTS2 + MTS3 + MTS4 + MTS5 + MTS6 + MTS7 + MTS8 + MTS9'
Fit.Strict <- cfa(Strict, data=MACS, group="Country", meanstructure=TRUE, std.lv=TRUE, group.equal=c("loadings","intercepts", "residuals"))
summary(Fit.Strict, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

#Does Scalar fit significantly better than Strict Factorial?
anova(Fit.Scalar, Fit.Strict)


###########OPTIMISM
###Individual Country Model
USA <- subset(MACS, Country==0)
NonUSA <- subset(MACS, Country==1)

Full <- 'OPT =~ Opt1 + Opt2R + Opt3 + Opt4R + Opt5R + Opt6'
Fit.USA <- cfa(Full, data=USA, meanstructure=TRUE, std.lv=TRUE)
Fit.NonUSA <- cfa(Full, data=NonUSA, meanstructure=TRUE, std.lv=TRUE)
summary(Fit.USA, fit.measures=TRUE, standardized=TRUE, ci=TRUE)
summary(Fit.NonUSA, fit.measures=TRUE, standardized=TRUE, ci=TRUE)


###Configural Model###
Config <- 'OPT =~ Opt1 + Opt2R + Opt3 + Opt4R + Opt5R + Opt6'
Fit.Config <- cfa(Config, data=MACS, group="Country", meanstructure=TRUE, std.lv=TRUE)
summary(Fit.Config, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

###Metric Model###
Metric <- 'OPT =~ Opt1 + Opt2R + Opt3 + Opt4R + Opt5R + Opt6'
Fit.Metric <- cfa(Metric, data=MACS, group="Country", meanstructure=TRUE, std.lv=TRUE, group.equal=c("loadings"))
summary(Fit.Metric, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

#Does Configural fit singificantly better than Metric?
anova(Fit.Metric, Fit.Config)

###Scalar Model
Scalar <- 'OPT =~ Opt1 + Opt2R + Opt3 + Opt4R + Opt5R + Opt6'
Fit.Scalar <- cfa(Scalar, data=MACS, group="Country", meanstructure=TRUE, std.lv=TRUE, group.equal=c("loadings","intercepts"))
summary(Fit.Scalar, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

#Does Metric fit significantly better than Scalar?
anova(Fit.Scalar, Fit.Metric)

###Strict Factorial Model
Strict <- 'OPT =~ Opt1 + Opt2R + Opt3 + Opt4R + Opt5R + Opt6'
Fit.Strict <- cfa(Strict, data=MACS, group="Country", meanstructure=TRUE, std.lv=TRUE, group.equal=c("loadings","intercepts", "residuals"))
summary(Fit.Strict, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

#Does Scalar fit significantly better than Strict Factorial?
anova(Fit.Scalar, Fit.Strict)









