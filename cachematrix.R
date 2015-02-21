# This file contains functions to cache inverse of square matrix 
# 
# Assumptions: inverse of a matrix can be calculated for the given square matrix.
#
# 2015-02-21

#
# makeCacheMatrix
#
# Method to create an instance of cacheMatrix along with methods to Cache (setsolve) 
# and retrieve the cached results
#


makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setminverse <- function(solve) mi <<- solve
        getminverse <- function() mi
        list(set = set, get = get,
             setminverse = setminverse,
             getminverse = getminverse)
}


# cacheSolve: 
#
# Returns a cached copy of the inverse of a square matrix
# if it does not find a cached copy, it computes the inverse using 'solve'
# and caches the data for future calls, before returning the results.
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getminverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data)
        x$setminverse(mi)
        mi
}
