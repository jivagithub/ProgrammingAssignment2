## This R script creates two functions to demonstrate how to avoid re-calculating
## potentially costly (in terms of processing) calculations.  To do this, we create
## functions to check for, retrieve, and store values in cached memory.

## makeCacheMatrix creates a special "vector", which is really a list containing 
## functions to:
##   -  set the value of a matrix
##   -  get the value of a matrix
##   -  set the value of a matrix's inverse
##   -  get the value of a matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # Set the inverse to NULL at the onset
        set <- function(y){
                x <<- y # Set the cached matrix to the matrix passed to the function
                i <<- NULL # Set the cached inverse to NULL
        }
        get <- function() x # Return the matrix
        setinverse <- function(solve) i <<- solve # Set the inverse of the matrix
        getinverse <- function() i # Return the inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve calculates the inverse matrix of the special "vector" created with 
## the makeCacheMatrix function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse matrix from the cache 
## and skips the computation. Otherwise, it calculates the inverse matrix of the 
## data and sets the value of the inverse matrix in the cache via the setinverse 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

