## The makeCacheMatrix function creates "neo" to cache its inverse. 
## The cacheSolve function computes the inverse.  
## If the inverse is already computed, it grabs the cache.

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
