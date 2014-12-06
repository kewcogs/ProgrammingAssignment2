## Inverting a matrix can be a computationaly expensive operation. 
## The functions here enable the inverse for a given matrix to be
## cached so that has only to be calculated at most once.
##
## Use makeCacheMatrix to create a matrix with a cacheable inverse
## Use cacheSolve to get the inverse

## Creates a matrix for use with 'cacheSolve'. Returns a 'special' vector
## which contains functions to:
##   set value of the matrix
##   get value of the matrix
##   set value of the inverse 
##   get value of the inverse

## Usage:
##   makeCacheMatrix(x)

## Arguments:
##   x      underlying matrix 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL         # a NULL inverse signfies not-yet-calculated
    
    set <- function(y){
        x <<- y
        inv <<- NULL    # any existing cached inverse now stale
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    # return 
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## Returns the inverse of a matrix. If the value for this matrix
## has already been calculated then a cached value is returned, 
## otherwise a new inverse in calculated.

## Usage:
##   cacheSolve(x, ...)

## Arguments:
##   x      a matrix created with 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()   # attempt to retrieve cached inverse
        
        if(!is.null(inv)){      # found cached inverse
            message("getting cached data")
            return (inv)
        }
        
        # find inverse of matrix and cache it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)       # write back to cache
        inv
}
