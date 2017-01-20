## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##creates a list containing functions to
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix's inverse
##get the value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## calculates the inverse of the matrix 
## First checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips further computation. 
## Otherwise computes the inverse and sets the value in the cache using setinv.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
