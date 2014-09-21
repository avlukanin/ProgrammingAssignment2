## Functions Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly 


## Creates a special "matrix" object that can cache its inverse.
## The matrix supplied should be invertible.

makeCacheMatrix <- function(x = matrix()) {
    ## sm is a private variable, which has the latest inverted matrix
    sm <- NULL
    
    ## the matrix can be redefined, e.g.
    ## a <- makeCacheMatrix(matrix(1:4, 2, 2))
    ## a$set(matrix(4:1, 2, 2))
    set <- function(y) {
        x <<- y
        ## the cache should be cleared in this case
        sm <<- NULL
    }
    
    ## get the original (or redefined with set()) matrix
    get <- function() x
    
    ## cache the matrix (save the result in the inner variable)
    setsmatrix <- function(smatrix) sm <<- smatrix
    
    ## get the cached inverted matrix
    getsmatrix <- function() sm
    
    ## return a list of functions, accessing the private variables
    list(set = set, get = get,
         setsmatrix = setsmatrix,
         getsmatrix = getsmatrix)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## check if the result is stored already in the cache
    m <- x$getsmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## otherwise...
    
    ## get the original martix
    data <- x$get()
    
    ## Inverse the matrix
    m <- solve(data, ...)
    
    ## Cache the result
    x$setsmatrix(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}