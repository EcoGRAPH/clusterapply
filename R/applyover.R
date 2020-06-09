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
                      settosplit=NULL,
                      nameaftersplit=NULL,
                      over=NULL,
                      cluster=NULL) {

  applyoverworker <- function(x=NULL,
                              applyfun=NULL,
                              applyargs=NULL,
                              settosplit=NULL,
                              nameaftersplit=NULL,
                              over=NULL,
                              cluster=NULL) {

    tryCatch({

      # only retain those data for this level of over
      tempdf <- settosplit[settosplit[,over] == x,]

      # create a new args to pass along the data as well
      myargs <- applyargs
      myargs[[nameaftersplit]] <- tempdf

      # run the call
      result <- do.call(what=applyfun,
                        args=myargs)

      # return the result
      return(result) },

      error = function(e) { return(e) } )

  }

  # if we aren't passed a cluster, make a clean environment
  if (is.null(cluster)) {

    cluster <- parallel::makeCluster(1)

  }

  # get list of the levels of the variable over which we split
  myx <- unique(settosplit[,over])

  # evaluate over this variable
  result <- parallel::clusterApplyLB(fun=applyoverworker,
                           cl=cluster,
                           x=myx,
                           applyfun=applyfun,
                           applyargs=applyargs,
                           settosplit=settosplit,
                           nameaftersplit=nameaftersplit,
                           over=over)

  # make sure we know which entry corresponds to which level of over
  names(result) <- myx

  return(result)

}
