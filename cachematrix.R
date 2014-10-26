## functions for creating, solving and caching the inverse of a matrix 


## Create a matrix that can contain a cached value for the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(
        set = set,
        get = get, 
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## get the inverse of a matrix using solve() and cache the result
## if a cached result exist return it instead of calculating it again
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
