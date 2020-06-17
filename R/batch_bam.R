batch_bam <- function(data=NULL,
                      bamargs=NULL,
                      bamargs_fallback=NULL,
                      over=NULL) {

  # run the requested model
  mymodels <- clusterapply::applyover(applyfun=mgcv::bam,
                        applyargs=bamargs,
                        fallbackargs=bamargs_fallback,
                        settosplit=data,
                        nameaftersplit="data",
                        over=over)

  return(mymodels)

}
