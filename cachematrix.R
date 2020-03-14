## The functions shown below allow you to create and cache the inverse of an array
#
## This function creates a special "matrix" object that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv<<-inverse
        getinverse <- function() inv
        list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)
}
#
## This function return a matrix that is the inverse of 'x'
#
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
