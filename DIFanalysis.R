###DIF Analysis in R###

#Set working directory
setwd("C:/Users/Dev Dalal/Dropbox/APSY 753--Spring 2017/Data Examples")

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

########## Make the data with a lack of invariance ######
make.data <- function(N){
  set.seed(1234)
  a <- matrix(abs(rnorm(15,1,.3)), ncol=1)
  d <- matrix(rnorm(15,0,.7),ncol=1)
  d1 <- d2 <- cbind(d, d-1, d-2) # b parameters for both groups
  d2[13:15, ] <- d1[13:15, ] + 1 # here is the DIF
  itemtype <- rep('graded', nrow(a))
  dataset1 <- simdata(a, d1, N, itemtype)
  dataset2 <- simdata(a, d2, N, itemtype)
  dat <- rbind(dataset1, dataset2)
  return(dat)
}
N <- 1000
dat <- make.data(N)
group <- c(rep('Ref', N), rep('Foc', N))
focal.data <- dat[1:1000,]
ref.data <- dat[1001:2000,]
############################################

########### check dimensionality ########
fa.parallel(focal.data)
fa.parallel(ref.data)
#########################################

########## check model fit ##############
foc.model <- mirt(focal.data, model = 1, itemtype = "graded", SE=TRUE)
M2(foc.model)
ref.model <- mirt(ref.data, model = 1, itemtype = "graded", SE=TRUE)
M2(ref.model)
foc.fit <- itemfit(foc.model)
foc.fit
ref.fit <- itemfit(ref.model)
ref.fit
### optional plotting
plots.foc <- list()
plots.ref <- list()
for(i in 1:length(dat)){
  plots.foc[[i]]<-itemfit(foc.model,empirical.plot = i)
  plots.ref[[i]]<-itemfit(ref.model,empirical.plot = i)
}
plots.foc
plots.ref
#########################################

################## Baseline Model ############################
model.constrained <- multipleGroup(dat, 1, group,
                                   invariance = c(colnames(dat), 'free_means', 'free_var'))
constrained.parameters <- coef(model.constrained,simplify = TRUE)[[1]][[1]]
constrained.parameters
##############################################################

####### First round of DIF analysesb - All Others As Anchors #######
dif.drop <- DIF(model.constrained, c('a1','d1','d2','d3'), scheme = 'drop',
                seq_stat = .05)
dif.drop
## use the optional function to table the output
dif.drop.out <-
  get.dif.items(out.list=dif.drop,p.val=.05,parms=constrained.parameters)
dif.drop.out
####################################################################

##### Run an anchor-item model #####
itemnames <- colnames(dat)
anc.items.names <- itemnames[c(2,8,9,10,12)]
test.items <- c(1,3:7,11,13:15)
model_anchor <- multipleGroup(dat, model = 1, group = group,
                              invariance = c(anc.items.names, 'free_means', 'free_var'))
anchor.parms <-coef(model_anchor,simplify = TRUE)[[1]][[1]]
anchor.parms
#####################################

#################### Final round of DIF analyses #################
dif.anchor <- DIF(model_anchor, c('a1','d1','d2','d3'), items2test =
                    test.items, plotdif = TRUE)
dif.anchor
## use the optional function to table the output
dif.anchor.out <-
  get.dif.items(out.list=dif.anchor,p.val=.05,parms=anchor.parms)
dif.anchor.out
#################################################################

################## Compute the effect sizes #####################
empirical_ES(model_anchor, DIF=FALSE) # test level stats
empirical_ES(model_anchor) # item level stats
empirical_ES(model_anchor, DIF=FALSE, plot=TRUE) # expected test score plots
empirical_ES(model_anchor, plot=TRUE) # expected item score plots
#################################################################





