## Cache the inverse matrix to avoid repeated calculation

## The first function creates a list to cache the calculated inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse2) {inverse <<- inverse2}
        getinverse <- function(){inverse}
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function check first whether the inverse matrix has already been calculated
## If calculated, it will directly use the cached value. If not, it will calculate
## the inverse matrix and remember the result

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
}
