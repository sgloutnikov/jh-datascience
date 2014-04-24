## Create a cacheble matrix type. Has the ability to cache the
## result and look it up in the future to improve performance.

## Create matrix type.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get matrix
  get <- function() x
  
  ## Set cached matrix
  setCache <- function(matinv) inv <<- matinv
  
  ## Get cached matrix
  getCache <- function() inv
  
  ## Return cachedmatrix type
  list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## Return inverse of cachedmatrix x. Looks up cache if available. 

cacheSolve <- function(x, ...) {
  inv <- x$getCache()
  
  ## Check if matrix has already been cached
  if(!is.null(inv)) {
    message("Getting cached matrix.")
    return(inv)
  }
  
  ## Get matrix
  data <- x$get()
  
  ## Solve matrix
  inv <- solve(data, ...)
  
  ## Set cache
  x$setCache(inv)
  
  ## Return inversed matrix
  inv
}
