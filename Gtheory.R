######G-Theory Variance Decomposition######

setwd("C:/Users/dd811711/Dropbox/APSY 753--Spring 2017/Data Examples")

#Install and Load Package
install.packages("gtheory", dependencies=TRUE)
library(gtheory)


###Unnested Design###
#Read in data
unnested <- read.csv("Unnested.csv", header=TRUE)

#Make Factors
unnested$Reflector_ID <- as.factor(unnested$Reflector_ID)
unnested$Reflection_ID <- as.factor(unnested$Reflection_ID)
unnested$Rater_ID <- as.factor(unnested$Rater_ID)

#Specify random equation
Unnest <- "Rating_Primary ~ (1|Reflector_ID) + (1|Rater_ID) + (1|Rater_ID:Reflector_ID)"

#Conduct variance decomposition
UNested <- gstudy(data=unnested, formula=Unnest)

#Conduct D-Study
dstudy(UNested, colname.objects="Reflector_ID", colname.scores="Rating_Primary", data=unnested)

###Nested Design###
#Read in data
nested <- read.csv("Nested.csv", header=TRUE)

#Make Factors
nested$Reflector_ID <- as.factor(nested$Reflector_ID)
nested$Reflection_ID <- as.factor(nested$Reflection_ID)
nested$Rater_ID <- as.factor(nested$Rater_ID)

#Specify random equation
nest <- "Rating_Primary ~ (1|Reflector_ID) + (1|Rater_ID) + (1|Rater_ID:Reflector_ID) + (1|Reflection_ID:Reflector_ID)"

#Conduct variance decomposition
Nested <- gstudy(data=nested, formula=nest)

#Conduct D-Study
dstudy(Nested, colname.objects="Reflector_ID", colname.scores="Rating_Primary", data=nested)









