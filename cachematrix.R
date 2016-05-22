# 2 functions have been built for caching computed inverse matrix.

## makeCacheMatrix stores the input matrix x, and its inverse inv, 
## when computed, into cache.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheResolve computes the inverse of a matrix x. Before computing, 
## it looks to see if the compute has been done and cached before. 
## If yes, it retieves the inverse inv from cache. 
## Otherwise, it computes the inverse inv.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

