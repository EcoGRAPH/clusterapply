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

applyoverworker <- function(x=NULL,
                            applyfun=NULL,
                            applyargs=NULL,
                            nameaftersplit=NULL,
                            over=NULL,
                            cluster=NULL) {

  tryCatch({

    # add arguments to pass along
    applyargs[[nameaftersplit]] <- x

    rm(x)
    rm(nameaftersplit)
    gc()

    # run the call
    result <- do.call(what=applyfun,
                      args=myargs)

    # clean up as much as we can
    rm(list=setdiff(ls(), "result"))
    gc()
    # return the result
    return(result)

  },

    error = function(e) {

      rm(list=setdiff(ls(), "e"))
      gc()
      return(e)

  } )

}
