applytoeachinlist <- function(listobject=NULL,
                              applyfun=NULL,
                              applyargs=NULL,
                              nameaftersplit=NULL,
                              splitalongside=NULL,
                              splitalongsidename=NULL,
                              splitalongsidesplitter=NULL,
                              libs=NULL,
                              cluster=NULL) {
  
  library(parallel)
  
  applytoeachinlistworker <- function(x=NULL,
                                      listobject=NULL,
                                      applyfun=NULL,
                                      applyargs=NULL,
                                      nameaftersplit=NULL,
                                      splitalongside=splitalongside,
                                      splitalongsidename=splitalongsidename,
                                      splitalongsidesplitter=splitalongsidesplitter,
                                      libs=NULL) {
    tryCatch({
      
      for (library in libs) {
        if (!require(library, character.only=T, quietly=T)) {
          install.packages(library, repos = "http://cran.us.r-project.org")
          library(library, character.only=T)
        }
      }
      
      # only retain that list object which needs to be evaluated
      tempobj <- listobject[[x]]
      
      # create a new args to pass along the data as well
      myargs <- applyargs
      myargs[[nameaftersplit]] <- tempobj
      if (!is.null(splitalongside)) {

        myargs[[splitalongsidename]] <- splitalongside[splitalongside[,splitalongsidesplitter] == x,]

      }
        
      # run the call
      result <- do.call(what=applyfun,
                        args=myargs)
      
      # return the result
      return(result) },
      
      error = function(e) { return(e) } )
    
  }
  
  # if we aren't passed a cluster, make a clean environment
  if (is.null(cluster)) {
    
    cluster <- makeCluster(1)
    
  }

  # get list of the levels of the variable over which we split
  myx <- names(listobject)
  if (!is.null(splitalongside)) {

    applyargs[[splitalongsidename]] <- splitalongside

  }
  
  # evaluate over this variable
  result <- clusterApplyLB(fun=applytoeachinlistworker,
                           cl=cluster,
                           x=myx,
                           listobject=listobject,
                           applyfun=applyfun,
                           applyargs=applyargs,
                           nameaftersplit=nameaftersplit,
                           splitalongside=splitalongside,
                           splitalongsidename=splitalongsidename,
                           splitalongsidesplitter=splitalongsidesplitter,
                           libs=libs)
  
  # make sure we know which entry corresponds to which level of over
  names(result) <- myx
  
  return(result)
  
}