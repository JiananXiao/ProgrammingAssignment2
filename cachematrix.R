## Two functions below are designed to cache and compute 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(mx = matrix()) {
    inverse_mx <- NULL
    set <- function(x) {
        mx <<- x;
        inverse_mx <<- NULL;
    }
    get <- function() 
    return(mx);
    setinv <- function(inv) 
    inverse_mx <<- inv;
    getinv <- function() 
    return(inverse_mx);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix function above.  
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mx, ...) {
    inverse_mx <- mx$getinv()
    if(!is.null(inverse_mx)) {
        message("Getting Cached Inverse Matrix Data...")
        return(inverse_mx)
    }
    data <- mx$get()
    invserse_mx <- solve(data, ...)
    mx$setinv(inverse_mx)
    return(inverse_mx)
}
