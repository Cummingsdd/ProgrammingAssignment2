## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    set_result <- function(val) cache <<- val
    get_result <- function() cache
    get_function <- function() fun
    list(set = set, get = get, 
         set_result = set_result,
         get_result = get_result)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'.
    ## If the result has already been calculated 
    ## the previous result will be returned
    m <- x$get_result()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_result(m)
    m
}
