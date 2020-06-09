summary.batch_bam <- function(models=NULL,
                              cluster=NULL) {

  # apply predict.gam to each object in the set with complete newdata
  mysummaries <- clusterapply::applytoeachinlist(listobject=models,
                                   applyfun="summary",
                                   nameaftersplit="object",
                                   cluster=mycluster)

  return(mysummaries)

}
