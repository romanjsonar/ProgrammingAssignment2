## Two functions work together to optimize inverse operations on big matrices
## A special structure is created to cache matrix inverse and compute it
## only when cached value doesn't exist or matrix value is different


## Function makeCacheMatrix
## description: creates a matrix object that caches matrix and its inverse
## arguments
##   m square invertable matrix
## returns
##   cached matrix object
## usage
##   makeCacheMatrix(m)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- y
        inv <<- NULL
    }
    get <- function() x
    getcache <- function() cache
    setcache <- function(y) cache <<- y
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(
        set = set, 
        get = get,
        getcache = getcache,
        setcache = setcache,
        setinverse = setinverse, 
        getinverse = getinverse)
}


## Function cacheSolve
## description: invert a cached square matrix
## arguments
##   x cached matrix object
## returns
##   inverse of a cached matrix
## usage
##   cacheSolve(x)

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    cache <- x$getcache()
    m <- x$get()
    if (!is.null(inv) 
        && is.matrix(m) 
        && is.matrix(cache) 
        && dim(m) == dim(cache) 
        && all(m == cache) 
        ) {
        message("getting cached data")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setinverse(inv)
    x$setcache(m)
    inv
}
