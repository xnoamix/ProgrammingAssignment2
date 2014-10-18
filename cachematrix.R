## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function receives as input an invertible matrix and return
## as output a list of 4 functions:
##	set - this function initialize the global variables:
##		variable x to be the matric given as input
##		variable m which is the cached inverse matrix to be NULL
##	get - a function returning the input matrix
##	setinv - this function set the global variable m to be the inverse matrix that was calculated
##	getinv - returns the cached inverse matrix m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
## This function returns the inverse matrix using the functions from makeCacheMatrix. It first check if the inverse matrix already stored
## in cache, then it will return the store value, else, it will calculate the inverse and will return the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
