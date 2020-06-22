#' A worker function for possible parallelization in applytoeachinlist
#'
#' @param X Internal - name of listobject which should be evaluated
#' @param listobject A named list object containing elements to which
#'   the applyfun will be applied.
#' @param applyfun The character name of a function to apply.
#' @param applyargs A named list of arguments supplied to applyfun.
#' @param splitalongside An optional dataframe that should be split alongside
#'   the listobject, containing the factor variable splitalongsidename.
#' @param nameaftersplit The name of the argument of applyfun that
#'   takes the settosplit dataframe after it has been split; this is
#'   commonly 'x' or 'data'
#' @param splitalongsidename The name of splitalongside after it has been split;
#'   e.g. this is commonly something like "newdata".
#' @param splitalongsidesplitter The name of the factor variable in settosplit, over
#'   which it should be split.
#' @return This function returns a named list of results, having applied
#'   applyfun to settosplit for every level of the 'over' variable. So for
#'   example, result[["a"]] is the result of applyfun(data[data$over == 'a']).

applytoeachinlistworker <- function(X=NULL,
                                    listobject=NULL,
                                    applyargs=NULL,
                                    applyfun=NULL,
                                    nameaftersplit=NULL,
                                    splitalongside=NULL,
                                    splitalongsidename=NULL,
                                    splitalongsidesplitter=NULL) {
  tryCatch({

    # only retain that list object which needs to be evaluated
    tempobj <- listobject[[X]]

    # create a new args to pass along the data as well
    myargs <- applyargs
    myargs[[nameaftersplit]] <- tempobj
    if (!is.null(splitalongside)) {

      myargs[[splitalongsidename]] <- splitalongside[splitalongside[,splitalongsidesplitter] == X,]

    }

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

      # clean up as much as we can
      rm(list=setdiff(ls(), "e"))
      gc()
      return(e)

  } )

}
