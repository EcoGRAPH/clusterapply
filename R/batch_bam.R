#' Run a set of separate but similar mgcv::bam models in a single function call, with
#' the possibility of replacing arguments if the regression results in an error.
#'
#' @export
#'
#' @param data The dataframe in which all the data for regressions are to be found.
#'   This dataframe should contain a column that splits it into separate frames
#'   over which mgcv::bam will be applied.
#' @param bamargs A named list of arguments to be passed to each bam. For example,
#'   this could be bamargs=list('formula' = 'y ~ te(x,t)', 'discrete' = TRUE)
#' @param bamargs_fallback If the regression defined by bamargs returns an error, any
#'   named arguments in this named list overwrite the arguments in bamargs and the
#'   regression is attempted again. for example, we could have the simpler model
#'   bamargs_fallback=list('formula' = 'y ~ s(x))'
#' @param over A character string giving the name of the column in data which is
#'   used to split and pass subsets of the data to bam.
#' @return This function returns a named list of bam objects, and possibly errors
#'   if bamargs and bamargs_fallback fail to produce a model for some levels of over.

batch_bam <- function(data=NULL,
                      bamargs=NULL,
                      bamargs_fallback=NULL,
                      over=NULL) {

  # run the requested model
  mymodels <- clusterapply::applyover(applyfun=mgcv::bam,
                        applyargs=bamargs,
                        fallbackargs=bamargs_fallback,
                        settosplit=data,
                        nameaftersplit="data",
                        over=over)

  return(mymodels)

}
