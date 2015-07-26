## This is a program of two functions. The first one (makeCacheMatrix) create  
## a matrix (it must be a square one) that can cache its inverse.
## The inverse its computed with the second function (cacheSolve) and
## stored in the cache.

## makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.

##Example of imput: 
##  a<- makeCacheMatrix(matrix(c(10,3,159,0), nrow=2, ncol=2)
##  cacheSolve(a)

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

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
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
