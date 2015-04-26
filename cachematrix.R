## This script contains my submission for R Programming
## Assignment #2. It constructs two functions for caching the inverse
## of a matrix.  The first is makeCacheMatrix and the second
## is cacheSolve.

## makeCacheMatrix creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL           ## Intitialize inverse as null
  set <- function(y) {  ## Set matrix & reset inverse to null
    x <<- y
    inv <<- NULL        
  }
  
  get <- function() x                           ##Return matrix
  setinv <- function(inverse) inv <<- solve(x)  ##Set cached inverse
  getinv <- function() inv                      ##Return cached inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above, but only if the inverse hasn't already been calculated 
## and the matrix has not changed). If the inverse is already cached, then 
## the cacheSolve retrieves the inverse  from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()   ##Load either null or cached data
  if(!is.null(inv)) {   ##Test if cached inverse exists 
    message("getting cached data")
    return(inv)    ##Return cached data
  }
  data <- x$get()   ##Otherwise, get matrix
  inv <- solve(data, ...)  ##Compute inverse
  x$setinv(inv)  ##Set inverse in the cache
  inv  ##Return new inverse
}
