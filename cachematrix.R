## Author       :       Joseph G. Noonan
## Class        :       RPROG-030
## Assignment   :       Programming Assignment 2
## Date         :       July 26, 2015

## makeCacheMatrix builds a list of functions to set and get a matrix and 
## set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inv to NULL
        inv <- NULL
        ## setMatrix assigns a matrix and sets the cached inv variable to NULL
        setMatrix <- function(y) {
                x <<- y
                inv <- NULL
        }
        ## getMatrix returns the matrix to the calling function
        getMatrix <- function() x
        ## setInverse sets the caching variable inv to the inverse of the matrix x
        setInverse <- function(solve) inv <<- solve
        ## getInverse returns the inversed matrix
        getInverse <- function() inv
        #the following line stores the four functions:
        list(getMatrix = getMatrix, setMatrix = setMatrix, 
             setInverse = setInverse, getInverse = getInverse)
        
        
}


## cacheSolve returns the inverse of a matrix and uses the cached version
## if one is available otherwise it calculates it.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ## if inv is NOT NULL, then there is a cached version that can be used
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if inv is NULL, then it needs to be calculated
        inv <- solve(x$getMatrix())
        x$setInverse(inv)
        ## return the inverse of the matrix
        x$getInverse()
}
