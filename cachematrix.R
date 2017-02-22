## Put comments here that give an overall description of what your
## functions do

## here the make function makeCacheMatrix takes an "a" matrix as an input and converts it to an
## special matrix object, get() is used to get that object, set inverse is used to find the 
## inverse , and getinverse() is used to get the inverse of the matrix.

makeCacheMatrix <- function(a = matrix()) {
  b <- NULL
  set <- function(x) {
    a <<- x
    b <<- NULL
  }
  get <- function() a
  
  setinverse <- function(solve) b <<- solve
  getinverse <- function() b
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is used to get the inverse of matrix from the cache if matrix is already solved
## otherwise calculate the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
