###MTS DIF Example###

#Set working directory
setwd("C:/Users/Dev Dalal/Dropbox/APSY 753--Spring 2017/Data Examples")

###Read in Data
dat <- read.csv("MTS.csv", header=TRUE)
dat <- dat[,-1]
Focal <- dat[1:300,]
Ref <- dat[301:600,]
group <- c(rep('Ref', nrow(Ref)), rep('Foc', nrow(Focal)))

#Install and Load Packages
install.packages("psych")
install.packages("lessR")
install.packages("devtools")

library("psych")
library("lessR")
library("devtools")

#https://github.com/philchalmers/mirt
install_github('philchalmers/mirt')

library('mirt')

######################################################################
# Functions
#######################################################################
get.dif.items <- function(out.list,p.val,parms){
  dif.items <- NULL
  non.dif.items <- NULL
  for(i in 1:length(out.list)){ # loop list of items
    chi.sq <- out.list[[i]][2,6] #2 groups, so second row, and 6th column
    df <- out.list[[i]][2,7]
    p <- out.list[[i]][2,8]
    i.name <- names(out.list[i])
    d <- c(i.name,chi.sq,df,p,parms[i,])
    if(p < p.val){
      dif.items <- rbind(dif.items,d)
    }else{
      non.dif.items <- rbind(non.dif.items,d)
    }
  }
  if (!is.null(dif.items)) {
    dif.items <- data.frame(dif.items, row.names = NULL)
    colnames(dif.items)[1] <- "item"
    colnames(dif.items)[2] <- "chi.sq"
    colnames(dif.items)[3] <- "df"
    colnames(dif.items)[4] <- "p"
  }
  if (!is.null(non.dif.items)) {
    non.dif.items <- data.frame(non.dif.items, row.names = NULL)
    colnames(non.dif.items)[1] <- "item"
    colnames(non.dif.items)[2] <- "chi.sq"
    colnames(non.dif.items)[3] <- "df"
    colnames(non.dif.items)[4] <- "p"
  }
  r.list <- list(dif_items = dif.items, no_dif = non.dif.items)
  return(r.list)
}
########################## END FUNCTIONS ########################

###Check Dimensionality within Groups
fa.parallel(Focal)
fa.parallel(Ref)

###Check Model Fit
foc.model <- mirt(Focal, model=1, itemtype="graded", SE=TRUE)
M2(foc.model, impute=10)
ref.model <- mirt(Ref, model=1, itemtype="graded", SE=TRUE)
M2(ref.model, impute=10)

foc.fit <- itemfit(foc.model)
foc.fit
ref.fit <- itemfit(ref.model)
ref.fit

###Baseline Model
model.constrained <- multipleGroup(dat, 1, group=group,
                                   invariance = c(colnames(dat), 'free_means', 'free_var'))
constrained.parameters <- coef(model.constrained,simplify = TRUE)[[1]][[1]]
constrained.parameters

###Finding Anchor Items: DIF Round 1
dif.drop <- DIF(model.constrained, c('a1','d1','d2','d3','d3'), scheme = 'drop',
                seq_stat = .05)
dif.drop
## use the optional function to table the output
dif.drop.out <-
  get.dif.items(out.list=dif.drop,p.val=.05,parms=constrained.parameters)
dif.drop.out

###Plots
empirical_ES(model.constrained, DIF=FALSE, plot=TRUE) # expected test score plots
empirical_ES(model.constrained, plot=TRUE) # expected item score plots


















































































