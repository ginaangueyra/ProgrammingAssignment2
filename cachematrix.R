## The following functions are intended to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    invmatrix <- NULL
    
    setmatrix <- function(y){
        x <<- y
        invmatrix <<- NULL
    }
    
    getmatrix <- function() x
    
    setinverse <- function(inverse) invmatrix <<- inverse
    
    getinverse <- function() invmatrix
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, 
                   getinverse = getinverse)
              
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    invmatrix <- x$getinverse()
    
    if(!is.null(invmatrix)){
        message("Getting inverse matrix")
        return(invmatrix)
    }
    
    initmatrix <- x$getmatrix()

    invmatrix <- solve(initmatrix)
    
    x$setinverse(invmatrix)
    
    invmatrix
    
}
