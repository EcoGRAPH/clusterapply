#' Run predictions on batch_lm models
#'
#' @export predict.batch_lm
#'
#' @param models This is a named list of bam models, probably resulting from
#'   batch_lm.
#' @param predictargs This is a named list of arguments for the predict function.
#' @param over The name of the column in newdata which contains the levels over
#'   which the batch_lm was originally split (and should correspond to the names
#'   of the listed models)
#' @param newdata The data frame to be passed to predict.lm after splitting by
#'   over. Currently, all predictions must be run anew - i.e. we do not access
#'   $fitted.
#' @return A vector with predictions in the order of rows of newdata.

predict.batch_lm <- function(models=NULL,
                             predictargs=NULL,
                             over=NULL,
                             newdata=NULL) {

  # if we have newdata, we need to split it carefully
  if (!is.null(newdata)) {

    # set up a lookup
    newdata$reserved_rownumber <- 1:nrow(newdata)

    # apply predict to each object in the set with complete newdata
    myfitted <- clusterapply::applytoeachinlist(listobject=models,
                                  applyfun=stats::predict.lm,
                                  applyargs=predictargs,
                                  nameaftersplit="object",
                                  splitalongside=newdata,
                                  splitalongsidename="newdata",
                                  splitalongsidesplitter=over)

    # get reordered list of rownumbers
    myx <- names(models)
    predframe <- data.frame()
    for (curx in myx) {

        tempdf <- data.frame(reserved_rownumber = newdata$reserved_rownumber[newdata[,over]==curx])
        predframe <- bind_rows(predframe, tempdf)

    }

    # add the reordered predictors
    predframe$preds <- unlist(clusterapply::applytoeachinlist(listobject=myfitted,
                                                              applyfun="as.data.frame",
                                                              nameaftersplit="x"),
                              use.names=FALSE)

    newdata <- dplyr::left_join(newdata, predframe, by="reserved_rownumber")
    return(unlist(newdata$pred, use.names=FALSE))

  } else {

    stop("predict.batch_lm requires newdata, even if newdata=trainingdata")

  }

}
