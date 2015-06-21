## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(original.matrix = matrix()) {
    
    if(!is.matrix(original.matrix)) {
        stop("Please give a matrix.")
    }
    inverted.matrix <- NULL
    
    set <- function(y) {
        original.matrix <<- y
        inverted.matrix <<- NULL
    }
    
    get <- function() {
        original.matrix
    }
    
    set.inverse <- function(solve) {
        inverted.matrix <<- solve
    }
    
    get.inverse <- function() {
        inverted.matrix
    }
    list (
        set = set
        , get = get
        , set.inverse = set.inverse
        , get.inverse = get.inverse
        )
}


## Write a short comment describing this function

cacheSolve <- function(cacheable.matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverted.matrix <- cacheable.matrix$get.inverse()
    if(!is.null(inverted.matrix)) {
        message("Getting cached inverse matrix")
        return(inverted.matrix)
    }
    matrix.to.inverse <- cacheable.matrix$get()
    inverted.matrix <- solve(matrix.to.inverse)
    cacheable.matrix$set.inverse(inverted.matrix)
    inverted.matrix
}
