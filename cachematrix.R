## Put comments here that give an overall description of what your
## functions do

###############################################################################
## Convert a standard square matrix into a alternative cacheable structure.
## The return value is a list structure, holding matrix information.
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solved) inv <<- solved
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
###############################################################################
## A function to solve the inverse of a square matrix x, which can cache the
##    calculated value, if the original matrix is identical (i.e., not null)
## The input value is an enhanced list-matrix (ar produced above).
## The return value is an atomic numerical matrix.
###############################################################################
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #message("start...")
  if( class(x) == "matrix") {
    message("Input should not be an atomic matrix class, but rather a enhanced list-matrix.")
    return()
  }
  inv <- x$getInverse()
  #message(paste(c("inv == ",inv)))
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }  else {
    message("no cached data available.")
  }
  #message("Past the cache check.")
  data <- x$get()
  #message(paste(c("Got the data:  ",data)))
  inv <- solve(data, ...)
  #message(paste(c("Got the inverse:  ",inv)))
  x$setInverse(inv)
  message("Inverse updated in the object.")
  inv
}
