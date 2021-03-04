#' Apply a function to each member of a list, possibly splitting a dataframe
#' alongside the list (e.g. to supply newdata to every member of a list)
#'
#' @export
#'
#' @param listobject A named list object containing elements to which the
#'   applyfun will be applied.
#' @param applyfun The character name of a function to apply.
#' @param applyargs A named list of arguments supplied to applyfun.
#' @param fallbackargs A named list of arguments to try in case the primary
#'   `applyargs` fail.
#' @param splitalongside An optional dataframe that should be split alongside
#'   the listobject, containing the factor variable splitalongsidename. For
#'   example, if listobject is a list of regressions, then splitalongside may be
#'   a set of newdata, indexed so that predictions for each regression depend
#'   only on some subset of splitalongside
#' @param nameaftersplit The name of the argument of applyfun that takes the
#'   settosplit dataframe after it has been split; this is commonly 'x' or
#'   'data'
#' @param splitalongsidename what should the splitalongside frame be called
#'   after it has been split? This is commonly something like "newdata"
#' @param splitalongsidesplitter The name of the factor variable in settosplit,
#'   over which it should be split.
#' @return This function returns a named list of results, having applied
#'   applyfun to settosplit for every level of the 'over' variable. So for
#'   example, applytoeachinlist(...)[["a"]] is the result of
#'   applyfun(listobject[["a"]]).

applytoeachinlist <- function(listobject=NULL,
                              applyfun=NULL,
                              applyargs=NULL,
                              fallbackargs=NULL,
                              nameaftersplit=NULL,
                              splitalongside=NULL,
                              splitalongsidename=NULL,
                              splitalongsidesplitter=NULL) {


  # get list of the levels of the variable over which we split
  myx <- names(listobject)

  # set up some holders
  result <- list()
  for (curx in myx) {

    tempapplyargs <- applyargs
    tempapplyargs[[nameaftersplit]] <- listobject[[curx]]
    if (!is.null(splitalongside)) {

      tempapplyargs[[splitalongsidename]] <- splitalongside[splitalongside[,splitalongsidesplitter]==curx,]

    }

    result[[curx]] <- tryCatch({

      # add to the list of results
      do.call(what=applyfun, args=tempapplyargs)

    }, error=function(e) {

      if (!is.null(fallbackargs)) {

        # if we have an error, try the fallbackargs
        for (curfallbackarg in 1:length(fallbackargs)) {

          tempapplyargs[[names(fallbackargs)[curfallbackarg]]] <- fallbackargs[[curfallbackarg]]

        }
        # add to the list of results
        do.call(what=applyfun, args=tempapplyargs)

      } else { return(NA) }

    })

    # clean up
    rm(tempapplyargs)
    gc()

  }

  # make sure names are correct
  names(result) <- myx

  # clean up
  rm(list=setdiff(ls(), "result"))

  gc()
  return(result)

}
