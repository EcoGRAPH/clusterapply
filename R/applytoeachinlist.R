#' Apply a function to each member of a list, in parallel.
#'
#' @param listobject A named list object containing elements to which
#'   the applyfun will be applied.
#' @param applyfun The character name of a function to apply.
#' @param applyargs A named list of arguments supplied to applyfun.
#' @param splitalongside An optional dataframe that should be split alongside
#'   the listobject, containing the factor variable splitalongsidename.
#' @param nameaftersplit The name of the argument of applyfun that
#'   takes the settosplit dataframe after it has been split; this is
#'   commonly 'x' or 'data'
#' @param over The name of the factor variable in settosplit, over
#'   which it should be split.
#' @param libs A vector of named libraries that need to be loaded
#'   to run applyfun in clean clusters (e.g. applyfun 'bam' requires
#'   libs = c('mgcv')
#' @param cluster A cluster created by parallel::makeCluster. If this
#'   is not provided, applyover will create a single-node cluster and
#'   run applyfun in serial over settotsplit.
#' @return This function returns a named list of results, having applied
#'   applyfun to settosplit for every level of the 'over' variable. So for
#'   example, result[["a"]] is the result of applyfun(data[data$over == 'a']).

applytoeachinlist <- function(listobject=NULL,
                              applyfun=NULL,
                              applyargs=NULL,
                              nameaftersplit=NULL,
                              splitalongside=NULL,
                              splitalongsidename=NULL,
                              splitalongsidesplitter=NULL) {


  # get list of the levels of the variable over which we split
  myx <- names(listobject)
  if (!is.null(splitalongside)) {

    applyargs[[splitalongsidename]] <- splitalongside

  }

  # run the function
  result <- lapply(X=myx,
                   FUN=clusterapply::applytoeachinlistworker,
                   listobject=listobject,
                   applyargs=applyargs,
                   applyfun=applyfun,
                   nameaftersplit=nameaftersplit,
                   splitalongside=splitalongside,
                   splitalongsidename=splitalongsidename,
                   splitalongsidesplitter=splitalongsidesplitter)

  # make sure we know which entry corresponds to which level of over
  names(result) <- myx

  # clean up as much as we can
  rm(list=setdiff(ls(), "result"))
  gc()

  return(result)

}
