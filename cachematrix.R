## The functions allow caching the inverse of a matrix such that
## the inverse is only calculated if it hasn't been calculated
## before. This works in two steps:

## 1) This function creates the "matrix" object that is able to 
## cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function() inv <<- solve(x)
        getInverse <- function() inv
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 2) This function checks whether the inverse has already been
## calculated and either loads the result or calculates the 
## inverse:

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting cached Inverse")
                return(inv)
        }
        data <- x$getMatrix()
        inv <- solve(data, ...)
        x$setInverse()
        inv
}
