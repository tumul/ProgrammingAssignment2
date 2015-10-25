## Matrix inversion is usually a costly computation. The following functions
## provide possibility of caching the inverse of a matrix rather than compute 
## it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## 1. Set the value of the matrix   
  set <-function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## 2. Get the value of the matrix  
  get <- function() x
  
  ## 3. Set the value of the inverse  
  setInverse <- function(inverse) i <<- inverse
  
  ## 4. Get the value of the inverse  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  
  ## Calling the getInverse() function for the inverse of the matrix
  i <- x$getInverse()
  
  ## Cache Hit   
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Cache Miss  
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  
  i
}