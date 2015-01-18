## This function wraps a matrix with functions allowing its inverse
## to be stored and retrieved. These functions are returned as members
## of a list
## 
## Input: invertible matrix
## Output: a list that wraps the matrix and exposes functions 
##         set/get, setinverse/getinverse
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

## This function returns the inverse of the matrix in makeCacheMatrix.
## If the inverse is already cached in makeCacheMatrix, it returns that
## value, otherwise it calculates the inverse and stored it in makeCacheMatrix.
##
## Input: makeCacheMatrix list
## Output: inverse of matrix wrapped by makeCacheMatrix
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
