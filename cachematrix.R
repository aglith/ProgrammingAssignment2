## Programming assignment 2
## The two functions: makeCacheMatrix() and cacheSolve() create
## and cache the inverse of a matrix, since matrix inversion is
## a costly computation

## makeCacheMatrix()  creates a special "matrix" object that can
## cache its inverse.
## Based on makeVector() from assigment instructions.

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) x_inverse <<- inverse
        getinverse <- function() x_inverse
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## cachSolve() computes the inverse of the special "matrix"
## returned by makeCacheMatrix()
## If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve retrieves the inverse
## from the cache.
## Based on cachemean() from assigment instructions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inverse <- x$getinverse()
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(x_inverse)
        }
        data <- x$get()
        x_inverse <- solve(data, ...)
        x$setinverse(x_inverse)
        x_inverse
}
