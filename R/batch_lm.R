batch_lm <- function(data=NULL,
                     lmargs=NULL,
                     over=NULL,
                     cluster=NULL) {

  # run the requested model
  mymodels <- clusterapply::applyover(applyfun=lm,
                        applyargs=lmargs,
                        settosplit=data,
                        nameaftersplit="data",
                        over=over,
                        cluster=cluster)

  return(mymodels)

}
