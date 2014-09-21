## Functions Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly 


## Creates a special "matrix" object that can cache its inverse.
## The matrix supplied should be invertible.

makeCacheMatrix <- function(x = matrix()) {
    sm <- NULL
    set <- function(y) {
        x <<- y
        sm <<- NULL
    }
    get <- function() x
    setsmatrix <- function(smatrix) sm <<- smatrix
    getsmatrix <- function() sm
    list(set = set, get = get,
         setsmatrix = setsmatrix,
         getsmatrix = getsmatrix)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    ## Inverse the matrix
    m <- solve(data, ...)
    
    ## Cache the result
    x$setsmatrix(m)
    
    ## Returns a matrix that is the inverse of 'x'
    m
}