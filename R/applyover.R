applyover <- function(applyfun=NULL,
                      applyargs=NULL,
                      settosplit=NULL,
                      nameaftersplit=NULL,
                      over=NULL,
                      libs=NULL,
                      cluster=NULL) {
  
  library(parallel)
  
  applyoverworker <- function(x=NULL,
                              applyfun=NULL,
                              applyargs=NULL,
                              settosplit=NULL,
                              nameaftersplit=NULL,
                              over=NULL,
                              libs=NULL,
                              cluster=NULL) {
    
    tryCatch({
      
      for (library in libs) {
        if (!require(library, character.only=T, quietly=T)) {
          install.packages(library, repos = "http://cran.us.r-project.org")
          library(library, character.only=T)
        }
      }
      
      # only retain those data for this level of over
      tempdf <- settosplit[settosplit[,over] == x,]
      
      # create a new args to pass along the data as well
      myargs <- applyargs
      myargs[[nameaftersplit]] <- tempdf
      
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
  myx <- unique(settosplit[,over])

  # evaluate over this variable
  result <- clusterApplyLB(fun=applyoverworker,
                           cl=cluster,
                           x=myx,
                           applyfun=applyfun,
                           applyargs=applyargs,
                           settosplit=settosplit,
                           nameaftersplit=nameaftersplit,
                           over=over,
                           libs=libs)
  
  # make sure we know which entry corresponds to which level of over
  names(result) <- myx
  
  return(result)
 
}