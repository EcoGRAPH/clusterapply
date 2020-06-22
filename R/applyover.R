#' Apply a function over a dataframe after splitting along some "over" variable
#'
#' @param applyfun The character name of a function to apply.
#' @param applyargs A named list of arguments supplied to applyfun.
#' @param fallbackargs A named list of arguments to replace arguments in
#'   applyargs if the call to applyfun results in an error. Any arguments
#'   in applyargs that are not named in fallbackargs will be reused.
#' @param settosplit A dataframe that will be split by the 'over'
#'   parameter below.
#' @param nameaftersplit The name of the argument of applyfun that
#'   takes the settosplit dataframe after it has been split; this is
#'   commonly 'x' or 'data'
#' @param over The name of the factor variable in settosplit, over
#'   which it should be split.
#' @return This function returns a named list of results, having applied
#'   applyfun to settosplit for every level of the 'over' variable. So for
#'   example, applyover(...)[["a"]] is the result of applyfun(data[data$over == 'a']).

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

      # add to the list of results
      result[[curx]] <- do.call(what=applyfun, args=tempapplyargs)

    }, error=function(e) {

      # if we have an error, try the fallbackargs
      for (curfallbackarg in 1:length(fallbackargs)) {

        tempapplyargs[[names(fallbackargs)[curfallbackarg]]] <- fallbackargs[[curfallbackarg]]

      }
      # add to the list of results
      result[[curx]] <<- do.call(what=applyfun, args=tempapplyargs)

    })

    # clean up
    rm(tempapplyargs)
    gc()

  }

  # clean up
  rm(list=setdiff(ls(), "result"))
  gc()
  return(result)

}
