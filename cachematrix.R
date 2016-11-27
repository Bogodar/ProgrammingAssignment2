## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        ## initialize the object s as a placeholder for a future value.
        s <- NULL
        ## clear the value of s each time x is reset, forcing calls to cacheSolve to recalculate the matrix inversion rather than retrieving wrong value from cache
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## define the getter for the matrix x
        get <- function() x
        ## define the setter for the matrix inversion s
        setsolve <- function(solve) s <<- solve
        ## define the getter for solve s
        getsolve <- function() s
        ## assign each of these functions an element within a list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## return an inverse matrix and assign it to s
        s <- x$getsolve()
        ## return s from cache if it is defined instead of computing a new one
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## istead calculate new inverse matrix
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
