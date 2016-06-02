## Put comments here that give an overall description of what your
## functions do

## Function that creates a "cached" matrix from a regular matrix
## in order to speed up calculation of matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solvedMatrix) m <<- solvedMatrix
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of a cache matrix if hasn't been
## calculated already, otherwise return the cached inverse matrix.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("Getting cached matrix data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
