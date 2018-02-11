## Put comments here that give an overall description of what your
## functions do
## Caching the inverse of a matrix
## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
     set <- function(y) {   
                x <<- y
                m <<- NULL
}
get <- function() x
        setcache <- function(INV) m <<- INV
        getcache <- function() m
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}

## Write a short comment describing this function
## This function computes the inverse of makeCacheMatrix above. 
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getcache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- INV(data, ...)
        x$setcache(m)
        m
}

