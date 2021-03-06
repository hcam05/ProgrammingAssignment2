## The functions create a matrix, inverts a matrix and caches the inverse.

## The makeCacheMatrix function creates "neo" to cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    
    neo <-NULL
    set <- function(y) {
        x <<- y
        neo <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) neo <<- solve
    getinverse <- function() neo
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function computes the inverse.  
## If the inverse is already computed, it grabs the cache.

cacheSolve <- function(x, ...) {
    neo <- x$getinverse()
    if(!is.null(neo)) {
        message("getting cached data")
        return(neo)
    }
    one <- x$get()
    neo <- solve(one, ...)
    x$setinverse(neo)
    neo
}