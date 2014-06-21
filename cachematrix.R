## A pair of functions that cache the inverse of a matrix.
## Programming Assignment2 from R Programming (Coursera).

## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inv <<- solve
    getSolve <- function() inv
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Computes the inverse of the special "matrix" return by makeCacheMatrix.
## If the inverse has already bben calculated (and the matrix has not 
## changed), then retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getSolve()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setSolve(inv)
    inv
}
