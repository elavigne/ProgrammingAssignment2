## Put comments here that give an overall description of what your
## functions do

## TODO: Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        # set a new matrix and reset the inverse to null
        # to avoid returning the cached inverse of the
        # previously set matrix.
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }

        # retrieve the matrix
        get <- function() x

        # cache the inverse for this matrix so it can
        # be retrieved later on rather than recalculated.
        setinverse <- function(inverse) inv <<- inverse

        # retrieve the inverse of the matrix from the cache;
        # if the inverse has not yet been cached, null is returned
        getinverse <- function() inv

        # return the cachedMatrix as a list of functions that can
        # be called on it
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## TODO: Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Retrieve the cached inverse of the matrix stored in x
        inv <- x$getinverse()

        # if the cached inverse is not null, return that value
        if(!is.null(inv)) {
          message("Getting cached data...")
          return(inv)
        }

        # if the cached inverse is null, retrieve the matrix
        # store in x and calculate its inverse
        data <- x$get()
        inv <- solve(data, ...)

        # cache the inverse of x
        x$setinverse(inv)

        # return the inverse
        inv
}
