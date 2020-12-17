###Computing Variances, Covariance, and Correlations by Hand###

#Setting a working directory
setwd("C:/Users/Dev Dalal/Dropbox/STAT 5105--Spring 2014/Lectures")

#Creating variables
Y <- c(71, 67, 69, 56, 53, 52, 59, 70, 75, 53)
X1 <- c(16, 18, 30, 18, 24, 12, 30, 32, 26, 14)

#Creating a data frame
Class1 <- data.frame(PTG = Y, SS = X1)

#Creating Deviation Scores
Class1$PTG.Dev <- (Class1$PTG - (mean(Class1$PTG)))
Class1$SS.Dev <- (Class1$SS - (mean(Class1$SS)))

#Creating Deviations Squared
Class1$PTG.DevSQ <- (Class1$PTG.Dev)^2
Class1$SS.DevSQ <- (Class1$SS.Dev)^2

#Summing Deviations Squared
Sum.PTG.DevSQ <- sum(Class1$PTG.DevSQ)
Sum.SS.DevSQ <- sum(Class1$SS.DevSQ)

#Variance of Variables
VarPTG <- Sum.PTG.DevSQ / (nrow(Class1)-1)
VarSS <- Sum.SS.DevSQ / (nrow(Class1)-1)
VarPTG
VarSS

#Standard Deviations of Variables
SD.PTG <- sqrt(VarPTG)
SD.SS <- sqrt(VarSS)
SD.PTG
SD.SS

#Creating Crossproducts
Class1$Cross.Product <- Class1$PTG.Dev * Class1$SS.Dev

#Covariance
CoV <- (sum(Class1$Cross.Product))/(nrow(Class1)-1)
CoV

#Correlation
r.xy <- (CoV/(SD.PTG * SD.SS))
r.xy

#Combining Results into a Single Object
Results <- cbind(VarPTG, VarSS, SD.PTG, SD.SS, CoV, r.xy)
Results

#Correlations Directly
#Should not require any specific packages if your R is up-to-date
COR <- cor.test(Class1$PTG, Class1$SS, type = "Pearson", conf.level = .95)
COR

