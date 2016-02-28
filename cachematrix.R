## In combination, makeCacheMatrix and cacheSolve add functionality to matrices
## allowing the inverse, an expensive calculation, to be cached for repeat access

## Returns a wrapper around a matrix adding cachable inverse functionality

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(new_inv) inv <<- new_inv
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the input CacheMatrix and caches the results
## Returns the already cached result if found

cacheSolve <- function(x, ...) {
  cached_inv <- x$getinverse()
  if(!is.null(cached_inv)) {
    message("getting cached data")
    return(cached_inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
