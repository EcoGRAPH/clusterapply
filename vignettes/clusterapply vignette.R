rm(list=ls())

library(parallel)
library(dplyr)
library(ggplot2)
library(clusterapply)

# create a fake data set with multiple levels of an over variable
nsamples <- 2500
mydf   <- data.frame(splittingvariable=sample(1:9, size=nsamples, replace=TRUE),
                     y=rnorm(nsamples))
mydf$z <- rnorm(n=nsamples,
                mean=2*mydf$y + mydf$y^2 + 3*mydf$splittingvariable,
                sd=0.5)
mydf$splittingvariable <- factor(mydf$splittingvariable)

# set some portion of these to NA so that we have a newdata to work with
mydf$z[sample(1:nrow(mydf),
              size=0.10*nrow(mydf),
              replace=FALSE)] <- NA

# create observation numbers for easier joining later
mydf$rowname <- as.character(1:nrow(mydf))
rownames(mydf) <- mydf$rowname

# create a cluster for all the functions below to use
mycluster <- makeCluster(2)

# this is equivalent to
mymodels <- batch_bam(data=mydf,
                      bamargs=list("formula"  = formula("z ~ s(y, bs='tp')"),
                                   "family"   = "gaussian",
                                   "discrete" = TRUE),
                      over="splittingvariable",
                      cluster=mycluster)

# take a look at model AICs
myAICs <- extractAIC.batch_bam(models=mymodels,
                               cluster=mycluster)
myAICs

# take a look at the summaries
mysummaries <- summary.batch_bam(models=mymodels,
                                 cluster=mycluster)
mysummaries[[1]]$scale

# create a new data frame for modeling
newdf <- expand.grid(splittingvariable=unique(mydf$splittingvariable),
                     y=seq(from=min(mydf$y, na.rm=TRUE),
                           to  =max(mydf$y, na.rm=TRUE),
                           length.out=500))

# predict on the new set
mypredictions <- predict.batch_bam(models=mymodels,
                                   predictargs=list("type"="response",
                                                    "discrete"=TRUE),
                                   over="splittingvariable",
                                   newdata=newdf,
                                   cluster=mycluster)
newdf$rowname <- rownames(newdf)
newdf <- left_join(newdf, mypredictions, by="rowname")

# join back to original frame and display
ggplot() + geom_line(data=newdf,
                     aes(x=y, y=pred,
                         color=factor(splittingvariable),
                         group=factor(splittingvariable)), size=1) +
  geom_point(data=mydf,
             aes(x=y, y=z, color=factor(splittingvariable),
                 group=factor(splittingvariable)), alpha=0.5) +
  scale_color_discrete(name="splitting\nvariable")

# turn off the cluster
stopCluster(mycluster)



