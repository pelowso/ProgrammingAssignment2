## The following functions are intended to cache the computation of an inverse matrix.
## While the contents of the source matrix are not changing the functions will provide the 
## previously cached value for its inverse matrix.
## These 2 functions work together as explained above each function
## An example of how to use them could be as follows:
##   1. Create an instance of makeCacheMatrix(x) for source matrix x: mcm <- makeCacheMatrix(x)
##   2. Get the inverse matrix of x by running cacheSolve against the new instance: cacheSolve(mcm)
##      - First time it's run the inverse matrix will be computed
##      - Next runs will provide the cached inverse matrix
##   3. Change the source matrix to 'y' by running the function set() from 'mcm': mcm$set(y)
##   4. Get the inverse matrix of y by running cacheSolve against the same instance: cacheSolve(mcm)
##      - As source matrix changed the inverse matrix will be computed again on the first run

## makeCacheMatrix(x) creates an "environment" (using lexical scoping) for the souce matrix 
## that keeps the following:
##   - the matrix itself: 'x'
##   - the inverse matrix: 'm'
##   - a function for setting the matrix 'x': set()
##      - It also reset the inverse matrix m because it's no valid anymore
##   - a function for setting the inverse matrix 'm': setinvmatrix()
##   - a function for getting the matrix 'x': get()
##   - a function for getting the inverse matrix 'm': getintmatrix()
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(invmat) m <<- invmat
        getintmatrix <- function() m
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getintmatrix = getintmatrix)
}

## cacheSolve(x) returns the inverse matrix of 'x' based on the following:
##   - 'x' is a makeCacheMatrix environment containing the source matrix
##   - if 'x' already has a computed inverse matrix 'm' it will be returned
##   - if 'x' does not have a computed matrix (is null) then:
##      - source matrix is obtained from 'x' and set locally into 'data'
##      - inverse matrix is computed and set locally into 'm'
##      - inverse matrix is saved into 'm' object from 'x' by using the setinvmatrix() from 'x'
##      - inverse matrix is returned from local 'm'
cacheSolve <- function(x, ...) {
        m <- x$getintmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmatrix(m)
        m
}
