## Author:  Grant Overcott
## Course:  R Programming by Johns Hopkins University
## Assignment: Programming Assignment 2: Lexical Scoping
## Date:  2/16/2016
## Description:  Two functions which cache the inverse of a matrix to save computation time.

## First function takes a matrix x as an argument
makeCacheMatrix <- function(x = matrix()) {
    ## inverse matrix is initialized to a NULL value indicating it is not yet cached
    mi <- NULL
    ## function to set input matrix y called outside this function (lexical scoping)
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    ## returns input matrix
    get <- function() x
    ## sets mi to the result of the inverse via solve function representing the cache
    setinverse <- function(inverse) mi <<- inverse
    ## returns the inverse matrix
    getinverse <- function() mi
    ## lists the possible functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function to replace solve which can retrieve a matrix inverse from a cache
cacheSolve <- function(x, ...) {
    ## Run the getinverse() function on mi
    mi <- x$getinverse()
    ## If there's a value, return that value and a cached message and exit the function
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    ## Otherwise get the original matrix
    data <- x$get()
    ## Set the inverse matrix 
    mi <- solve(data)
    ## Set the cache for next time
    x$setinverse(mi)
    ## Return the value for this time
    mi
}

##
##  Test code:  This code can be used to test the functions after they've been loaded into the local workspace
## 
##  This sets the matrix to a 9x9 matrix of random integers
##  test1$set(matrix(sample.int(81,9*9),9,9))
##
##  Returns the 9x9 matrix
##  test1$get()
##
##  Matrix multiplies the text matrix with it's cacheSolved inverse which is calculated this first time
##  test1$get() %*% cacheSolve(debug1)
##
##  Uses the round function to return more aesthetic identity matrix, and this time the cached inverse is used
##  round(test1$get() %*% cacheSolve(debug1))
##