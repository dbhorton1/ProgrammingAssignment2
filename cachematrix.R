## makeCacheMatrix and cacheSolve are paired functions that
## generate and report the inverse of an input matrix 
## (assumed to be square and invertible) or, if the inverse
## matrix has already been generated, report that inverse matrix 
## directly from cache.

## makeCacheMatrix creates a special list of functions that:
##   1) set the value of a matrix ("set")
##   2) get the value of a matrix ("get")
##   3) set the value of the inverse of the matrix ("setmatrix")
##   4) get the value of the inverse of the matrix ("getmatrix")

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve either 1) reports the inverse of the matrix from  
## makeCacheMatrix, if the inverse is already stored in cache,  
## or 2) generates the inverse of the matrix from makeCacheMatrix
## using the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
