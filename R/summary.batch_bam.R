#' Run summaries on batch_bam models
#'
#' @param models This is a named list of bam models, probably resulting from
#'   batch_bam.
#' @return A named vector with containing model summaries

summary.batch_bam <- function(models=NULL) {

  # apply predict.gam to each object in the set with complete newdata
  mysummaries <- clusterapply::applytoeachinlist(listobject=models,
                                   applyfun="summary",
                                   nameaftersplit="object")

  return(mysummaries)

}
