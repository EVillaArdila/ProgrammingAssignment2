## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## ----------------------------------------------------------------------
## The function "makeCacheMatrix" creates a "matrix", which
## is a list containing the functions: "set", "get","setsolve","getsolve"
## ----------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set the value of the matrix
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    ## set the value of the inverse
    setsolve <- function(solve) m <<- solve
    ## get the value of the inverse
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

## --------------------------------------------------------------------
## The function "cacheSolve" computes the inverse of the special "matrix"
## created with the function "makeCacheMatrix". It first check to see if
## the inverse has been already computated. Otherwise, it calculates the
## inverse of the matrix and sets the value of the inverse in the cache
## via the "makeCacheMatrix" function.
## --------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## get the value of the inverse
    m <- x$getsolve()
    ## If the inverse has been calculated, returns the cached value
    if(!is.null(m)){
        message("Getting cached Data")
        return(m)
    }
    ## Otherwise, it calculates the inverse of the matrix
    data <- x$get()
    m <- solve(data, ...)
    ## and sets the value of the inverse in the cache
    x$setsolve(m)
    m
}

