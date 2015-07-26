## Author       :       Joseph G. Noonan
## Class        :       RPROG-030
## Assignment   :       Programming Assignment 2
## Date         :       July 26, 2015

## makeCacheMatrix builds a list of functions to set and get a matrix and 
## set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        setMatrix <- function(y) {
                x <<- y
                inv <- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        #the following line stores the two functions:
        list(getMatrix = getMatrix, setMatrix = setMatrix, 
             setInverse = setInverse, getInverse = getInverse)
        
        
}


## cacheSolve returns the inverse of a matrix and uses the cached version
## if one is available otherwise it calculates it.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(x$getMatrix())
        x$setInverse(inv)
        x$getInverse()
}
