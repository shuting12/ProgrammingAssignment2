## Put comments here that give an overall description of what your
## functions do
# The overall function can generate inverse of the matrix when there is a matrix exist. 


## Write a short comment describing this function
#the makeCacheMatrix function creates a special "matrix" which can cache its inverse matrix, 
#includes: set the value of the matrix, get the value of the matrix, 
#set the value of the inverse matrix, get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## this cacheSolve function calculates the inverse matrix for the special matrix returned by the makeCacheMatrix function
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}