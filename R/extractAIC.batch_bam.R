extractAIC.batch_bam <- function(models=NULL,
                                 cluster=NULL) {
  
  source("applytoeachinlist.R")

  # apply predict.gam to each object in the set with complete newdata
  myAICs <- applytoeachinlist(listobject=models,
                              applyfun="extractAIC",
                              nameaftersplit="fit",
                              cluster=mycluster)
  
  # fix this nonsense later
  myAICs <- data.frame(t(data.frame(myAICs)))
  myAICs$model <- names(models)
  rownames(myAICs) <- names(models)
  
  return(myAICs)
  
}