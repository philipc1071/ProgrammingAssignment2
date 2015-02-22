## Matrix inversion is usually a costly computation and there can be some benefit to caching
## the matrix inverse rather than computing it each time. These functions use the solve function.

## This function creates a special "matrix" object that can cache its inverse so it doesn't have 
## to be recomputed each time
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinv <- function(solve) m <<- solve
   getinv <- function() m
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)   
}


## This function computes the inverse of the special "matrix" from makeCacheMatrix.  If it's 
## already been created before, it will obtain it from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinv()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinv(m)
   m
}
