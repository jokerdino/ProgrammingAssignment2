## I followed the methodology of the example function and replaced mean with
## solve function.

## makeCacheMatrix function finds the inverse of the matrix and assigns to a
## variable (cache). The function also exposes 4 public methods to get and set
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i

    list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

## cacheSolve function checks if the inverse of the matrix is already cached.
## If the cache is null, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()

    if(!is.null(i)) {
        message("gettingcached data")
        return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)

    i
}
