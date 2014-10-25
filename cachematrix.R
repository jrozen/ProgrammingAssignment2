## Solve a matrix and cache the value for future use

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


## solve a a matrix and cache the result

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
