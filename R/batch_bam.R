batch_bam <- function(data=NULL,
                      bamargs=NULL,
                      over=NULL,
                      cluster=NULL) {

  # run the requested model
  mymodels <- applyover(applyfun="bam",
                        applyargs=bamargs,
                        settosplit=data,
                        nameaftersplit="data",
                        over=over,
                        libs=c("mgcv"),
                        cluster=mycluster)

  return(mymodels)

}
