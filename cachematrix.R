## Matrix inversion is usually a costly computation.
## The two functions below, say makeCacheMatrix and cacheSolve, create a scheme of cache for the matrix inversion. 
## By building a special object (through the makeCacheMatrix function), the matrix can be stored beside its inverse.
## When asking for computing the inverse of a matrix (as a special object), through the cacheSolve function, 
## such a computation may be bypassed in the case that this same matrix was previously processed.

## The makeCacheMatrx function creates a special object, which may contain a basic matrix object as well as its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(actualInv) inv <<- actualInv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function gives the inverse of the matrix contained in the special object (created through makeCacheMatrix function):  
## if the inverse of the matrix is already available in the special object, it is promptly returned; 
## else,  the inverse of the  matrix is computed and, therefore, cached in the special object for a further use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
