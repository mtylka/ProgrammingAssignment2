## Together these two functions create a matrix and cache the inverse of said matrix
## 

## makeCacheMatrix creates a matrix that can cache the inverse of said matrix through solve function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix produced by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m     
}

## Return a matrix that is the inverse of 'x'
