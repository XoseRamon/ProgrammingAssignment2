## The general idea is to build a Matrix that caches its own inverse
## To do so we rely on two functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix
# wraps over a given matrix (x), which can be modified and obtained
# through the get and set functions
# Provides the methods getInverse and setInverse to get and set the 
# inverse of the contained matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) m <<- inverse
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve
# receives as a parameter a 'wrapped' matrix with makeCacheMatrix and
# returns its inverse. If the matrix has not its inverse stored, also 
# calculates it and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}
