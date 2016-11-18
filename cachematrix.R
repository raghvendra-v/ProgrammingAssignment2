## Finding inverse of a big matrix is a frequent operation, and at the same time
## it is hardware intensive
## Using these operations , the inverse of the matrix is cached upon first request.
## Subsequent fetch requests for inverse are returned from the cache, without 
## wasting time in calculations

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## convenience operations get and set provide ways to fetch and update the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv.mat <- NULL
    set <- function (new.mat) {
        x <<- new.mat
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(inv.matrix) inv.mat <<- inv.matrix 
    getSolve <- function() inv.mat
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv.mat <- x$getSolve()
    if(!is.null(inv.mat)) {
        #message to the user informing that result is coming from cache
        message("getting cached data") 
        return(inv.mat)
    }
    data <- x$get()
    inv.mat <- solve(data, ...)
    x$setSolve(inv.mat)
    inv.mat
}
