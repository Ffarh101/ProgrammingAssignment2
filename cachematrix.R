## Caching a matrix and retrieving the inverse of it
##==================================================

## Cache a matrix 
makeCacheMatrix <- function(x = matrix()) {
  
  cacheMat <- NULL
  
  setMat <- function(y) {
    x <<- y
    cacheMat <<- NULL
  }
  
  getMat <- function() x
  
  setCache <- function(inverse) cacheMat <<- inverse
  
  getCache <- function() cacheMat
  
  list(setMat = setMat,
       getMat = getMat,
       setCache = setCache,
       getCache = getCache)
}

## Retrieve inverse of a matrix from the cache
cacheSolve <- function(x, ...) {
  
  cacheMat <- x$getCache()
  
  if (!is.null(cacheMat)) {
    message("getting cache matrix...")
    return(cacheMat)
  }
  
  data <- x$getMat()
  cacheMat <- solve(data, ...)
  x$setCache(cacheMat)
  cacheMat
}
