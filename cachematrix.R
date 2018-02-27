## Put comments here that give an overall description of what your
## functions do

## creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
        invMatrix <-  NULL
        
        set <- function(y){
                matrix <<- y
                invMatrix <<- NULL
        }
        
        get <- function() matrix
        
        setInverse <- function(solve) invMatrix <<- solve
        
        getInverse <- function() invMatrix
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}

## is function computes the inverse of the special matrix returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invMatrix <- x$getInverse()
        
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setInverse(invMatrix)
        invMatrix
}
