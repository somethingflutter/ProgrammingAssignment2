## Assignment 2: Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than compute it repeatedly.
##These functions compute and cache the inverse of a matrix.

## makeCacheMatrix creates a sprcial vector, which is a list of functions for the following:
##  set the value of the matrix
##  get the value of the matric
##  set the value for the inverse of the matrix
##  get the value for the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #variable that stores the inverse value
    set <- function(y){
        x <<- y
        inv <<- NULL  #Clear the inverse when the matrix has been changed
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){ #there is a cached value to return
        message("getting cached data")
        return(inv)
    }
    #otherwise, calculate and cache the value before returning it
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}