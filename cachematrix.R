## These functions are meant for implementing cached version of
## solve function. The solution is based on a special kind of matrix
## that can cache the results of the solve function.
##
## Example usage:
## matrx = makeCacheMatrix()
## B = matrix( c(1, 2, 3, 4),nrow=2, ncol=2)
## matrx$setMatrix(B)
## result = cacheSolve(matrx)
## print(result)

## makeCacheMatrix function creates a matrix that can store results of 
## matrix inverse in cache.
## Functions available in created matrix: setMatrix, getMatrix, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    setMatrix <- function(y) {
        m <<- y
        i <<- NULL
    }
    getMatrix <- function() m
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function calculates matrix inverse. If result has already
## been calculated and cached then function results cached result instead
cacheSolve <- function(x, ...) {
    
    i <- x$getInverse()
    
    ## Check if cached result exists
    if(!is.null(i)) {
        return(m)
    }
    data <- x$getMatrix()
    
    ## calculate inverse matrix
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

## Function for testing the functionality of the cached matrix
testCachedMatrix <- function() {
    matrx = makeCacheMatrix()
    B = matrix( c(1, 2, 3, 4),nrow=2, ncol=2)
    matrx$setMatrix(B)
    result = cacheSolve(matrx)
    print(result)    
}
