#Cached matrix inverse
#Author : Sergey Prokudin

#bunch of functions to get and set values of matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) x_inv <<- solve
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#calculate matrix inverse or get it from cache if it already have been calculated
cacheSolve <- function(x, ...) {
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
        x_inv
}
