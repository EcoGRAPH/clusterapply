#' Run predictions on batch_bam models
#'
#' @export predict.batch_bam
#'
#' @param models This is a named list of bam models, probably resulting from
#'   batch_bam.
#' @param predictargs This is a named list of arguments for the predict.bam function,
#'   such as list('type'='response').
#' @param over The name of the column in newdata which contains the levels over
#'   which the batch_bam was originally split (and should correspond to the names
#'   of the listed models)
#' @param newdata The data frame to be passed to predict.bam after splitting by
#'   over. Currently, all predictions must be run anew - i.e. we do not access
#'   $fitted.
#' @return A vector with predictions in the order of rows of newdata.

predict.batch_bam <- function(models=NULL,
                              predictargs=NULL,
                              over=NULL,
                              newdata=NULL) {

  # if we have newdata, we need to split it carefully
  if (!is.null(newdata)) {

    # we need to know which rows of the original data frame correspond to which predictions,
    # but since we're splitting the original data frame along the "over" variable, we might
    # not preserve the order of the rows. We create this variable called "reserved_rownumber"
    # so that we can reconstruct the order of the predictions to match the order of the original
    # data frame
    newdata$reserved_rownumber <- 1:nrow(newdata)

    # apply predict to each object in the set with complete newdata
    myfitted <- clusterapply::applytoeachinlist(listobject=models,
                                  applyfun=mgcv::predict.bam,
                                  applyargs=predictargs,
                                  nameaftersplit="object",
                                  splitalongside=newdata,
                                  splitalongsidename="newdata",
                                  splitalongsidesplitter=over)

    # get reordered list of rownumbers
    myx <- names(models)
    predframe <- data.frame()
    for (curx in myx) {

      # extract the reserved_rownumbers corresponding to these rows from the original data frame
      # according to level of x.
      tempdf <- data.frame(reserved_rownumber = newdata$reserved_rownumber[newdata[,over]==curx])
      predframe <- dplyr::bind_rows(predframe, tempdf)

    }

    # add the reordered predictors
    predframe$preds <- unlist(clusterapply::applytoeachinlist(listobject=myfitted,
                                                              applyfun="as.data.frame",
                                                              nameaftersplit="x"),
                              use.names=FALSE)

    # now, join back to the original data set so that we can get the predictions in the right order
    newdata <- dplyr::left_join(newdata, predframe, by="reserved_rownumber")
    return(unlist(newdata$pred, use.names=FALSE))

  } else {

    stop("predict.batch_bam requires newdata, even if newdata=trainingdata")

  }

}
