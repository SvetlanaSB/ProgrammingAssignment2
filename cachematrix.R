## Programming Assignment: Caching the Inverse of a Matrix

## This function includes list of functions to get and set original and inversed matrixes.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y){
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv_x <<- solve
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function creates inversed matrix only if it's not cached. 
## Otherwise it will get inversed matrix from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinv()
        if(!is.null(inv_x)){
                message("getting cached inversed matrix")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data)
        x$setinv(inv_x)
        inv_x
}
