#' Extract the AICs from batch_bam models.
#'
#' @param models This is a named list of lm models, probably resulting from
#'   batch_lm.
#' @return A data frame with columns corresponding to the outputs of extractAIC()
#'   called on an individual model.

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
