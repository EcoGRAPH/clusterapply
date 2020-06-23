#' Extract the AICs from batch_bam models.
#'
#' @export extractAIC.batch_bam
#'
#' @param models This is a named list of bam models, probably resulting from
#'   batch_bam.
#' @return A data frame with columns corresponding to the outputs of extractAIC()
#'   called on an individual model. Because these are sometimes defined differently
#'   depending on the arguments originally passed to the bam models, these are
#'   difficult to standardize. However, typically the first numerical column will
#'   be the degrees of freedom and the second will be the AIC.

extractAIC.batch_bam <- function(models=NULL) {

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
