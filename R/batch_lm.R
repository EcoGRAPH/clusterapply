#' Run a set of separate but similar stats::lm models in a single function call.
#'
#' @export
#'
#' @param data The dataframe in which all the data for regressions are to be found.
#'   This dataframe should contain a column that splits it into separate frames
#'   over which mgcv::bam will be applied.
#' @param lmargs A named list of arguments to be passed to each bam. For example,
#'   this could be bamargs=list('formula' = 'y ~ x + t', 'singular.ok' = FALSE)
#' @param over A character string giving the name of the column in data which is
#'   used to split and pass subsets of the data to lm.
#' @return This function returns a named list of lm objects, and possibly errors
#'   if lm fails to produce a model.

batch_lm <- function(data=NULL,
                     lmargs=NULL,
                     over=NULL) {

  # run the requested model
  mymodels <- clusterapply::applyover(applyfun=stats::lm,
                        applyargs=lmargs,
                        settosplit=data,
                        nameaftersplit="data",
                        over=over)

  return(mymodels)

}
