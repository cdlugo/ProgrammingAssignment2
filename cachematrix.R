## In combinaiton, makeCacheMatrix and cacheSolve add functionality to matrices
## allowing the inverse, an expensive calculation, to be cached for repeat access

## Returns a wrapper around a matrix adding cachable inverse functionality

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(new_matrix) {
    matrix <<- new_matrix
    inverse <<- NULL
  }
  get <- function() matrix
  setinverse <- function(new_inverse) inverse <<- new_inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the input CacheMatrix and caches the results
## Returns the already cached result if found

cacheSolve <- function(cache_matrix, ...) {
  inverse <- cache_matrix$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- cache_matrix$get()
  inverse <- solve(data, ...)
  cache_matrix$setinverse(inverse)
  inverse
}
