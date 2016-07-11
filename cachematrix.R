## These two functions help saving computation time when solving
## the inverse of a matrix. The calculation is performed only once, 
## the result is cached and can be retrieved when needed.


## 1) makeCacheMatrix: caches the matrix x given in argument,
## then returns an object consisting of a list of set/get functions
## allowing to retrieve or replace the cached matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set.matrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get.matrix <- function() x
    set.inverse <- function(y) inverse <<- y
    get.inverse <- function() inverse
    
    list(set.matrix = set.matrix, 
         get.matrix = get.matrix,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## 2) cacheSolve: expects an object x corresponding to a cached matrix,
## checks if the inverse matrix has already been solved and cached, 
## if not, solves the inverse matrix, caches it and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    y <- x$get.inverse()
    if (!is.null(y)){
        message("Getting cached data")
        return(y)
    }
    m <- x$get.matrix()
    y <- solve(m)
    x$set.inverse(y)
    y
}
