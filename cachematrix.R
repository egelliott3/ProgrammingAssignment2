## These functions, in conjunction, provide an optimized way
##  to compute the inverse of a matrix.

## Creates a function list for the provided matrix that will
##  compute the inverse of the matrix.  The list also
##  provides functions to support set/get on the original
##  matrix as well as set/get for computing the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solves the provided catch matrix function.  If the inverse of the
##  matrix x has already been solved it will return the cached value.
##  Otherwise it will compute the inverse, store it in the cache and
##  return the computed inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Reading from cache.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
