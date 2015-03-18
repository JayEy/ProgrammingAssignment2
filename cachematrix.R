## These functions allow the computation of the inverse of a matrix effectively. To avoid computational redundancy, the function
## "makeCacheMatrix"creates a special object of the matrix passed as the argument, that can cache its inverse.
## When called, passing the special vector previously created, the function "cacheSolve" check if the inverse of the matrix in
## the argument has not already been computed and set in the cache, and if so it return the result. Otherwise it computes it.

#---------------------------------------------#

## The "makeCacheMatrix" function creates a special matrix object that can cache its inverse
## The "set" object set the cached value of the matrix x, entered as an input, and clears inv if already existent in the environment
## The "get" object returns the value of "x".
## The "setinverse" object set the cached value of the inverse of the matrix "x".
## The "getinverse" object returns the value of "inv" (inverse of the matrix "x")
## The whole "makeCacheMatrix" object then returns a list with the value of "set,"get", "setinverse" and "getinverse"

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
                
        get <- function() x
        
        setinverse <- function(solve) inv <<- solve
        
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      
}

#---------------------------------------------#

## The "cacheSolve" function returns a matrix that is the inverse of the special matrix object created with the "makeCacheMatrix"
## function. The inverse matrix is either computed or retrieved from the cache if already computed in a previous iteration of the function.
## First, "inv" takes the value of the "getinverse()" element from the list "x"
## Then, if the "inv" object is not null (that is, if a previous version exists in the cache), then the function returns the cached value.
## Else, the "data" object receives the matrix (stored in the "get" element) and the function computes its inverse and store it in the cache
## using the "setinverse" element from "x".

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
