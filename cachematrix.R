## Code: cachematrix.R
## Author: elenapatricia218
## Date: May 22, 2014
## Purpose: Leveraging the lexical scoping nature of R
##   to cache computations which would otherwise take a long time to compute

## Section 1: makeCacheMatrix
## Create a matrix which caches the inverse of itself using the solve function
## Enable access to free variables defined in a "child" context
##   from a function in the "parent" environment
##   by leveraging the <<- operator

makeCacheMatrix <- function(x = matrix()) { #defines a matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve #returns the inverse of a matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Section 2: cacheSolve
## Access the cached inverse of the above matrix
## Referencing the above sets and gets

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## thanks to Gregory D. Horne, Community TA for instructions
## on how to test the code for accuracy, great tip!

## Results:
## amatrix=makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
## amatrix$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(amatrix)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## as expected based on input into http://www.bluebit.gr/matrix-calculator/