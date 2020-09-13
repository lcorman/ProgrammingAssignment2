## Functions for solving and caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Function to set the object's matrix data
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Function to retrieve the object's matrix data
  getMatrix <- function() x
  ## Function to set the object's matrix inverse
  setInverse <- function(inverse) inv <<- inverse
  ## Function to retrieve the object's matrix inverse
  getInverse <- function() inv
  ## Return list of 4 functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  ## Check if inverse has already been cached
  if(!is.null(inv)) {
    message("getting cached data")
    ## If already cached, simply return the inverse
    return(inv)
  }
  ## If not, get the matrix data
  data <- x$getMatrix()
  ## Solve for the matrix inverse
  inv <- solve(data, ...)
  ## Set the object's inverse
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
