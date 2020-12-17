###Some Basic Operations###

#(1) Installing packages
install.packages("psych", dependencies = TRUE)

#(2) Loading packages
library(psych)

#(3) Setting a working directory
setwd("C:/Users/Dev Dalal/Dropbox/STAT 5105--Spring 2014/Lectures")

#(4) Creating variables
Y <- c(71, 67, 69, 56, 53, 52, 59, 70, 75, 53)
X1 <- c(16, 18, 30, 18, 24, 12, 30, 32, 26, 14)

#(5) Creating a data frame
Class1 <- data.frame(PTG = Y, SS = X1)

#(6) Referring to a specific variable
Class1$PTG
Class1$SS

#(7.1) Adding a new variable to the frame directly
Class1$Cond <- c(1,1,1,1,1,2,2,2,2,2)

#(7.2) Adding a new variable to the frame as a fucntion
Class1$PTGDev <- (Class1$PTG - 62.5)
Class1$SSDev <- Class1$SS - (mean(Class1$SS))

#(8) Creating a factor
Class1$Cond <- factor(Class1$Cond, levels = c(1,2), labels = c("Control", "Treatment"))

#(9) Importing a data file
Class2 <- read.delim("Class2.dat", header = TRUE)

#(10) Obtaining descriptives
#Needs psych package
DescY <- describe(Class1$PTG)
DescX <- describe(Class1$SS)
DescAll <- describe(cbind(Class1$PTG, Class1$SS))

#(11) Obtaining descriptives by factor
#If you don't create a factor then you won't get labels
DescBy <- by(Class1, INDICES = Class1$Cond, FUN = describe)

#(12) Rounding Objects
DescY <- round(DescY, digits = 3)

#(13) Mean and sum of rows
Class1$Mean <- rowMeans(cbind(Class1$PTG, Class1$SS), na.rm = TRUE)
Class1$Sum <- rowSums(cbind(Class1$PTG, Class1$SS), na.rm = TRUE)

#(14) Mean and sum of columns
MeanPTG <- mean(Class1$PTG)
MeanSS <- mean(Class1$SS)
SumPTG <- sum(Class1$PTG)
SumSS <- sum(Class1$SS)

#(15) Conditionals
Class1$X4 <- ifelse(Class1$Cond == "Control", 0, 1)

#(16) Saving the Data File
write.table(Class1, "Class1.txt", sep = "\t" , row.names = FALSE)
