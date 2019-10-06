


## Assignment: Caching the Inverse of a Matrix


## Functions that cache the inverse of matrix
## Create matrix object
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Inverse property
  i <- NULL
  
  ## Setting the Matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Retrieve Matrix
  get <- function() {
    ## Return Matrix
    m
  }
  
  ## Set Inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get inverse
  getInverse <- function() {
    ## Return inverse
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Compute the inverse of the martrix or return

cacheSolve <- function(x, ...) {
  
  ## Return inverse of matrix
  m <- x$getInverse()
  
  ## Return inverse if set
  if( !is.null(m) ) {
    message("retrieving cached data")
    return(m)
  }
  
  ## Retrieve Matrix
  data <- x$get()
  
  ## Calculate inverse
  m <- solve(data) %*% data
  
  ## Set inverse
  x$setInverse(m)
  
  ## Return matrix
  m
}