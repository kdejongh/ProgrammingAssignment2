## Put comments here that give an overall description of what your
## functions do
# This source file contains 2 functions that are used to cache the inverse of a square matrix:
#   'makeCacheMatrix':  creates a list wrapped around a matrix that contains functions to set/get the matrix data 
#                       and set/get the matrix inverse
#   'cacheSolve':       accepts a list created by 'makeCacheMatrix' and retrieves the inverse of the wrapped matrix.
#                       The inverse is cached until the data of the wrapped matrix is changed.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # Wrapper function around a matrix that caches the matrix inverse.
    #
    # Args:
    #   x: the matrix that is wrapped, defaults to an empty matrix
    #
    # Returns:
    #   a list of 4 functions to:
    #       set/change the data of the wrapped matrix
    #       get the data of the wrapped matrix
    #       set the inverse of the wrapped matrix
    #       get the (cached) inverse of the wrapped matrix
    
    # Error handling
    if (!is.matrix(x) | (ncol(x) != nrow(x))) {
        stop("'x' must be a square matrix")
    }
    
    # initialize the inverse to 'NULL'
    inv <- NULL
    
    set <- function(y) {
        # Changes the data of the wrapped matrix x, the cached inverse is thereby reset to 'NULL'.
        # Error handling
        if (!is.matrix(y) | (ncol(y) != nrow(y))) {
            stop("'y' must be a square matrix")
        }
        x <<- y      # set the data
        inv <<- NULL # reset the (cached) inverse
    }
    get <- function() {
        # Retrieves the wrapped matrix.
        return(x) 
    }
    setInverse <- function(newInv) { 
        inv <<- newInv 
    }
    getInverse <- function() { 
        # Retrieves the cached inverse of the wrapped matrix; if no inverse was cached yet, 'NULL' is returned.
        return(inv)
    }
    
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # Returns the inverse of the matrix wrapped by 'x' (created by 'makeCacheMatrix'). The inverse is calculated the first time
    # this function is called for a matrix and the calculated inverse is cached for following calls. 
    # If the data of the matrix is changed the cached inverse will be reset to 'NULL' and calculated 
    # 
    # Args:
    #   x: a list created by 'makeCacheMatrix' that wraps a matrix and contains functions to get and cache 
    #       the inverse of that matrix
    #
    # Returns:
    #   The inverse of the matrix wrapped by 'x'
    
    # Error handling
    if (!is.list(x)) {
        stop("'x' must be a list")
    }
    if (is.null(x$getInverse) | is.null(x$setInverse) | is.null(x$get)) {
        stop("'x' is missing functions to get/set the inverse or get the matrix data.", 
             " Call 'makeCacheMatrix' to create the proper argument")
    }
    
    inv <- x$getInverse()
    if (is.null(inv)) {
        inv <- solve(x$get(), ...)
        # cache the result
        x$setInverse(inv)
    } else {
        message("getting cached inverse")
    }
    return(inv)
}
