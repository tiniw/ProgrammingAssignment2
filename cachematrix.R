## Programming assignment 2 - cacheable matrix that can cache its inverse
## Two functions: makeCacheMatrix that creates a cacheable matrix
## and cacheSolve that either gets the inverse of a matrix from cache or calculates it

## make a matrix that can cache its inverse; returns the functions to get and set
## the matrix and its inverse. The matrix is stored in x and its inverse in m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solves for inverse of CacheMatrix x either from cache or by calculating it

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