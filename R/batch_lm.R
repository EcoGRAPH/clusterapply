batch_lm <- function(data=NULL,
                     lmargs=NULL,
                     over=NULL) {

  # run the requested model
  mymodels <- clusterapply::applyover(applyfun=lm,
                        applyargs=lmargs,
                        settosplit=data,
                        nameaftersplit="data",
                        over=over)

  return(mymodels)

}
