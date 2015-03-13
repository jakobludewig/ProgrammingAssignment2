## The first function defined serves to create a matrix implementation which is interfaced by setter/getter functions and
## the second function is used to calculate the inverse of the matrix within the previously defined matrix structure, using
## cached values for the inverse when possible.

## makeCacheMatrix: creates a matrix-like structure with the ability to cache its own inverse. The function
##                  returns a list which contains the getter/setter functions for the matrix/inverse. The 
##                  values for the matrix and its inverse are stored within the environment of makeCacheMatrix.
##      'x' : the value of the matrix, defined in the environment of makeCacheMatrix
##      'inv': the inverse value cached within the environment of makeCacheMatrix, set to NULL if the inverse
##             needs to be updated or the inverse has not been calculated yet
##      'get': getter function for the matrix
##      'set': setter function for the matrix
##      'setinverse': setter function for the inverse
##      'getinverse': getter function for the inverse

makeCacheMatrix <- function(x = matrix()) {
    # the inverse has not been calculated yet, so set it to NULL
    inv <- NULL
    
    # the setter function for the matrix, also invalidates the inverse by setting it to NULL if the matrix
    # is updated
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # the getter function for the matrix
    get <- function() x
    
    # the setter function for the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # the getter function for the inverse
    getinverse <- function() inv
    
    # return a list of the functions defined previously, serving as the interface to the matrix implementation
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: takes matrix structure as defined by makeCacheMatrix and calculates its inverse. If the inverse has
##             not been calculated before, it calculates it using 'solve' and caches it within the matrix (i. e. the
##             environment of makeCacheMatrix). If the inverse has been calculated before and is still valid, the
##             cached version will be used.

cacheSolve <- function(x, ...) {
    # get the inverse from the matrix structure
    inv <- x$getinverse()
    
    # check whether the inverse is valid (!=NULL). If it is valid return the inverse and stop the execution of the
    # function
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # get the matrix
    data <- x$get()
    
    # invert the matrix
    m <- solve(data, ...)
    
    # store the inverse within the matrix object
    x$setinverse(m)
    
    # return the inverse
    m
}
