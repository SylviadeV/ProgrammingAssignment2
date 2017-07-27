## makeCacheMatrix makes a list of functions to
## set the value
## get the value
## set the value of inverse
## get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NUL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve returns the inverse of the created matrix. 
## First a check if the inverse has already been computed. 
## If yes, then it gets the result without computation.
## If no, it computes the inverse and sets the cache with setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("retrieving data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
 }
