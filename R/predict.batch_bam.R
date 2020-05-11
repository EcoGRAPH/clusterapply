predict.batch_bam <- function(models=NULL,
                              predictargs=NULL,
                              over=NULL,
                              newdata=NULL,
                              cluster=NULL) {

  # if we have newdata, we need to split it carefully
  if (!is.null(newdata)) {

    # apply predict.gam to each object in the set with complete newdata
    myfitted <- applytoeachinlist(listobject=models,
                                  applyfun="predict",
                                  applyargs=predictargs,
                                  nameaftersplit="object",
                                  splitalongside=newdata,
                                  splitalongsidename="newdata",
                                  splitalongsidesplitter=over,
                                  libs=c("mgcv"),
                                  cluster=mycluster)

  } else {

    # apply predict.gam to each object in the set with complete newdata
    myfitted <- applytoeachinlist(listobject=models,
                                  applyfun="predict",
                                  applyargs=predictargs,
                                  nameaftersplit="object",
                                  libs=c("mgcv"),
                                  cluster=mycluster)

  }

  # put predictions back into a data frame
  mypredictions <- data.frame(rowname = unlist(applytoeachinlist(listobject=myfitted,
                                               applyfun="names",
                                               nameaftersplit="x",
                                               cluster=mycluster)),
                              pred    = unlist(applytoeachinlist(listobject=myfitted,
                                                                applyfun="as.data.frame",
                                                                nameaftersplit="x",
                                                                cluster=mycluster)))


  return(mypredictions)

}
