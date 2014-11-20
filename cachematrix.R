# ==========================================================================
#  This R script contains functions that implement cached matrix inversion.
#  For large matrices, the inversion calculation can be computationally 
#  costly, thus the need to cache the matrix inverse the first time it is
#  computed for a particular matrix. During any subsequent calls to invert
#  the same matrix, the cached inverse will be returned instead of 
#  performing a new matrix inversion computation.
# ==========================================================================

## makeCacheMatrix ##############################
# Creates a special cache matrix, which is a list 
# containing a function to perform the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse
#################################################
makeCacheMatrix <- function(x = matrix()) {
    inv    <- NULL           # initialize the value of the inverse to be null
    setmat <- function(y) {       # set the value of the special cache matrix
        x   <<- y
        inv <<- NULL
    }
    getmat <- function() x        # get the value of the special cache matrix
    setinv <- function(inverse) inv <<- inverse # set value of matrix inverse
    getinv <- function() inv                    # get value of matrix inverse
    list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}

## cacheSolve ###################################
# Computes the inverse of the cache matrix 'x' if
# not previously computed, otherwise it returns
# the cached inverse matrix
# Arguments: special cache matrix, 'x' and 
#            arguments to be passed to the solve 
#            function when computing the matrix 
#            inverse
# Return:    a matrix that is the inverse of 'x'
#################################################
cacheSolve <- function(x, ...) {
    inv <- x$getinv()   # initialize value for matrix inverse
    if(!is.null(inv)) { # if inverse previously computed and saved
        message("Retrieving matrix inverse from cache")
        return(inv)             # return inverse
    }
    message("Computing matrix inverse")
    mat <- x$getmat()        # get matrix
    inv <- solve(mat, ...)   # compute matrix inverse
    x$setinv(inv)            # save matrix inverse in cache
    inv                      # return inverse 
}