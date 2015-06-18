## makeCacheMatrix returns a set of functions that enable
##  cacheSolve to retrieve a cached result once the inverse matrix
##  is calculated. Tested with:
##
##  y <- rbind(c(1, -1/4), c(-1/4, 1))
##  x <- makeCacheMatrix(y)
##  cacheSolve(x)

## makeCacheMatrix takes a matrix as input and output a list
##  of functions (set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve checks to see if the inverse matrix has already
##  been cached, if not it calculates the inverse and stores it
##  in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
