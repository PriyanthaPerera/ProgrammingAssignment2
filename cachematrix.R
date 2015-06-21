## Inverting a  matrix could be a costly computation. If a matrix has to be inverted
## repeatedly within a loop, the inverse can be cached in the first go round,
## and retreived for subsequent computations in the loop

## The following two functions create a matrix object (makeCacheMatrix) and executes
## the solve function if inverse not computed before or retrieves the cached version
## (cacheSolve) respectively 

## The following function creates a special matrix object, which is a list containing
## a function to set the value of the matrix. get the value of the matrix, set the 
## value of the inverse and get the value of the inverse

##Note that for this exercise it is assumed that the input matrix x is non-singular/invertible

makeCacheMatrix <- function(x = matrix()) {
              inverseMatrix <- NULL
              set <- function(y) {
                      x <<- y
                      inverseMatrix <<- NULL
              }
              get <- function() x
              setinverse <- function(solve) inverseMatrix <<- solve
              getinverse <- function() inverseMatrix
              list(set = set, get = get,
                   setinverse = setinverse, getinverse = getinverse)
              
    }


## cacheSolve generates the inverse of the special matrix created by makeCacheMatrix.
## It checks to see if the inverse has already been calculated, if so retrieves it
## from the cache and stops the computation. Otherwise calculates the inverse, sets
## the inverse matrix in the cache and prints the inverse

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
                                message("getting cache matrix")
                                return(inverseMatrix)
                              }
                                matrix <- x$get()
                                inverseMatrix <- solve(matrix, ...)
                                x$setinverse(inverseMatrix)
                                inverseMatrix
  }
