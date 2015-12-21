## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix and cacheSolve can be used together to calculate the inverse
## of a matrix and cache the result such that the inverse only has to be 
## calculated once.
## Write a short comment describing this function
## makeCacheMatrix creates a list that provides functionality to get and set 
## the cached value of calculating the inverse of a matrix 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(new.x) {
    x <<- new.x
    inverse <<- NULL
  }
  get <- function() { x }
  set.inverse <- function(new.inverse) { inverse <<- new.inverse }
  get.inverse <- function() { inverse }
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## Write a short comment describing this function
## cacheSolve when given a list such as created by the makeCacheMatrix function
## can calculate the inverse of the matrix and cache the result such that it 
## only has to be calculated once.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get.inverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set.inverse(inverse)
  inverse
}
