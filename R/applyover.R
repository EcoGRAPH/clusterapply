#' Apply a function over a dataframe in parallel.
#'
#' @param applyfun The character name of a function to apply.
#' @param applyargs A named list of arguments supplied to applyfun.
#' @param settosplit A dataframe that will be split by the 'over'
#'   parameter below.
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

applyover <- function(applyfun=NULL,
                      applyargs=NULL,
                      fallbackargs=NULL,
                      settosplit=NULL,
                      nameaftersplit=NULL,
                      over=NULL) {

  # set up some holders
  result <- list()

  for (curx in unique(settosplit[,over])) {

    # create temporary arguments, since these might change
    tempapplyargs <- applyargs
    tempapplyargs[[nameaftersplit]] <- settosplit[settosplit[,over] == curx,]

    tryCatch({

      tempresult <- do.call(what=applyfun, args=tempapplyargs)

    },

      error=function(e)

    {

      # if we have an error, try the fallbackargs
      for (curfallbackarg in 1:length(fallbackargs)) {

        tempapplyargs[[names(fallbackargs)[curfallbackarg]]] <- fallbackargs[[curfallbackarg]]

      }
      tempresult <- do.call(what=applyfun, args=tempapplyargs)

    })

    # add to the list of results
    result[[curx]] <- tempresult

    # clean up
    rm(tempresult)
    rm(tempapplyargs)
    gc()

  }

  # clean up
  rm(list=setdiff(ls(), "result"))
  gc()
  return(result)

}
