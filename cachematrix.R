## makeCacheMatrix defines and returns four functions for storing and retrieving the matrix and its inverse. 
## The inverse is stored in a variable that is scoped to the function. It performs no calculations or caching.
## cacheSolve performs the caching and calculation, using makeCacheMatrix as a storage mechanism.


## makeCacheMatrix provides an interface for storing and retrieving the matrix and its inverse
## the set function may be used to reset the matrix and the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## gets the inverse of a matrix that has been constructed using makeCacheMatrix.
## The x input parameter is the makeCacheMatrix function, whose internal functions are used to 
## get and set the matrix and its inverse.
## cacheSolve checks to see if an inverse exists, and if not, generates it and stores it in makeCacheMatrix.
## if the inverse already exists, then it returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
