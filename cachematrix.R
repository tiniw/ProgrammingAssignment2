## Put comments here that give an overall description of what your
## functions do

## make a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solves for inverse of matrix x either from cache or by calculating it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}