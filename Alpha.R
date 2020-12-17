######Alpha in R######

setwd("C:/Users/dd811711/Dropbox/APSY 753--Spring 2017/Data Examples")

##Install and load packages
install.packages("psych", dependencies=TRUE)
library(psych)

##Read in and subset data
mydata <- read.csv("EFA Example.csv", header=TRUE)

MS_Scale <- mydata[,c(1:13)]
MTS_Scale <- mydata[,c(11:19)]

##Run Alpha
  #alpha(x, keys=NULL,cumulative=FALSE, title=NULL, max=10,na.rm = TRUE, 
    #check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,n.obs=NULL)

alpha(MS_Scale, title = "Maximization Scale", na.rm=TRUE)
alpha(MTS_Scale, title = "Maximizing Tendency Scale", na.rm=TRUE)



