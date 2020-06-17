predict.batch_bam <- function(models=NULL,
                              predictargs=NULL,
                              over=NULL,
                              newdata=NULL) {

  # if we have newdata, we need to split it carefully
  if (!is.null(newdata)) {

    # set up a lookup
    row.names(newdata) <- 1:nrow(newdata)
    newdata$reserved_rownumber <- 1:nrow(newdata)

    # apply predict to each object in the set with complete newdata
    myfitted <- clusterapply::applytoeachinlist(listobject=models,
                                  applyfun=mgcv::predict.bam,
                                  applyargs=predictargs,
                                  nameaftersplit="object",
                                  splitalongside=newdata,
                                  splitalongsidename="newdata",
                                  splitalongsidesplitter=over)

    # put predictions back into a data frame
    mypredictions <- data.frame(reserved_rownumber = as.numeric(unlist(clusterapply::applytoeachinlist(listobject=myfitted,
                                                                                 applyfun="names",
                                                                                 nameaftersplit="x"))),
                                pred    = unlist(clusterapply::applytoeachinlist(listobject=myfitted,
                                                                                 applyfun="as.data.frame",
                                                                                 nameaftersplit="x")))

    # feels unwieldy, but think is necessary
    newdata <- dplyr::left_join(newdata, mypredictions, by=c("reserved_rownumber"))
    return(newdata$pred)

  } else {

    stop("predict.batch_bam requires newdata, even if newdata=trainingdata")

  }

}
