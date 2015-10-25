##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly 

## A pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse: 


## 1) This function creates a special "matrix" object that can cache its solve.

makeCacheMatrix <- function(x = matrix()) {
    #m <- matrix(nrow=nrow(x),ncol=ncol(x))
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve<- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}



## 2) This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the solve of 'x'
    mx <- x$getsolve()
    if(!is.null(mx)) {
        message("getting cached data")
        return(mx)
    }
    data <- x$get()
    mx <- solve(data, ...)
    x$setsolve(mx)
    mx
}
