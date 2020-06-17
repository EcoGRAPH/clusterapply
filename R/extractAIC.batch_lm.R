extractAIC.batch_lm <- function(models=NULL) {

  # apply predict.gam to each object in the set with complete newdata
  myAICs <- clusterapply::applytoeachinlist(listobject=models,
                              applyfun="extractAIC",
                              nameaftersplit="fit")

  # fix this nonsense later
  myAICs <- data.frame(t(data.frame(myAICs)))
  myAICs$model <- names(models)
  rownames(myAICs) <- names(models)

  return(myAICs)

}
