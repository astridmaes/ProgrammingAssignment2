makeCacheMatrix <- function(x = matrix())  {
  ## creates a special "vector", which is really a list containing a function to set the value of the matrix,
  ## get the value of the matrix, set the inverse of the matrix, get the inverse of the matrix
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve #Uses solve function to get the inverse of matrix
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv) ## List of functions
}

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

    m <- x$getinv() ## if the inverse exists, it gets it.
    if(!is.null(m)) { ## Exists in cache
      message("getting cached data")
      return(m) ## Result cached matrix, function finishes here
    }
    data <- x$get()
    m <- solve(data, ...)  # calc inverse
    x$setinv(m) ## Save inverse in cache
    m ## Result newly calculated
    
   }
