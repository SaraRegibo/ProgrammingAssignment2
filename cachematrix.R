###########################################################
# Programming Assignment 2: Caching the Inverse of a Matrix
###########################################################

# Creates a special "matrix", which is really a list containing a function to:
#   (1) set the value of the (invertible) matrix
#   (2) get the value of the (invertible) matrix
#   (3) set the value of the inverse matrix
#   (4) get the value of the inverse matrix

makeCacheMatrix <- function(matrix = matrix()) {
    # Cached value of the inverse matrix
    inverseMatrix <- NULL
    
    # (1) set the value of the (invertible) matrix
    setMatrix <- function(originalMatrix) {
        matrix <<- originalMatrix
        inverseMatrix <<- NULL
    }
    
    # (2) get the value of the (invertible) matrix
    getMatrix <- function() matrix
    
    # (3) set the value of the inverse matrix
    setInverse <- function(solve) inverseMatrix <<- solve(matrix)
    
    # (4) get the value of the inverse matrix
    getInverse <- function() inverseMatrix
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Calculates the inverse of the special "matrix" created by the above function.
# It first checks to see whether the inverse has already been calculated. If
# so, it gets the inverse matrix from the cache and skips the computation.  
# Otherwise, it calculates the inverse of the (invertible) matrix and sets the 
# value of the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(cacheMatrix, ...) {
    inverseMatrix <- cacheMatrix$getInverse()
    
    # The inverse has been calculated before
    # => take the value from the cache and skip the computation
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    
    # The inverse has NOT been calculated before
    # =>  calculate the inverse matrix and store it in the cache
    data <- cacheMatrix$getMatrix()
    inverseMatrix <- solve(data, ...)
    cacheMatrix$setInverse(inverseMatrix)
    inverseMatrix
}
