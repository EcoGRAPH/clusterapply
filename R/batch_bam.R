batch_bam <- function(data=NULL,
                      bamargs=NULL,
                      over=NULL,
                      cluster=NULL) {

  # run the requested model
  mymodels <- clusterapply::applyover(applyfun=mgcv::bam,
                        applyargs=bamargs,
                        settosplit=data,
                        nameaftersplit="data",
                        over=over,
                        cluster=cluster)

  return(mymodels)

}
