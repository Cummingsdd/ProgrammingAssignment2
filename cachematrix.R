## The following functions provide a mechanism to store a matrix and the result of a (presumably) time 
## consuming expression on that matrix in a single data structure. 


## The makeCacheMatrix function will take a matrix and encapsulate it in a list
## that consists of the original matrix, an empty result, and functions to get 
## and set these two.

makeCacheMatrix <- function(x = matrix()) {
    #initilize the cached result
    cache <- NULL 
    #setter for the input value
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    #getter for the input value
    get <- function() x
    #setter for function result
    set_result <- function(val) cache <<- val
    #getter for result
    get_result <- function() cache
    # List object storing the setter and getter functions
    list(set = set, get = get, 
         set_result = set_result,
         get_result = get_result)
}


## cacheSolve takes a "makeCacheMatrix" list, x, containing an invertible matrix and parameters for the 
##"solve" function. If the inverse has been calculated it will be returned, else it will be found, stored
## in x and the returned.

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
