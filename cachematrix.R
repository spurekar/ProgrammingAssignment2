## This file contains two functions makeCacheMatrix, and cacheSolve which are 
## used to calculate and store the inverse of a matrix. Once the inverse has
## been calculated, it is cached for easy access and to avoid re-calculation.

## It was originally created for Assignment 2 of the Coursera R Programming 
## class offered by Johns Hopkins University.

## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function()
    {
        x
    }
    setInverse <- function(inverse)
    {
        inv <<- inverse
    }
    getInverse <- function()
    {
        inv
    }
    list(set = set, get=get,
         setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## and returns it. If the inverse has already been calculated, cacheSolve
## retrieves the inverse from the cache and returns it.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv))
        {
            message("Getting cached inverse")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setInverse(inv)
        inv
}
