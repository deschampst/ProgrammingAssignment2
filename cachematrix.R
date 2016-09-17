## These functions work together to find and cache the inverse of a matrix.

## makeCacheMatrix() creates a special "matrix", which is really just a list
## containing a function to (1) set the value of the matrix, (2) get the
## value of the matrix, (3) set the inverse of the matrix, (4) get the
## inverse of the matrix. 

## cacheSolve() computes the inverse of the special "matrix" returned by
## makeCacheMatrix(). If the inverse has already been calculated (and the
## matrix hasn't changed), then cacheSolve() retrieves the inverse from
## the cache.

## makeCacheMatrix creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
