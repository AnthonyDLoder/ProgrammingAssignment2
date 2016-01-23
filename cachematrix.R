## These functions are used to create a special "matrix" and cache its inverse to reduce on computation time

## makeCacheMatrix creates a special "matrix" and stores the inverse of this "matrix"
makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## cacheSolve will check to see if the inverse of the "matrix" has been calculated and cached
## by makeCacheMatrix. If it has, it will retrieve this cached inverse. If not, it will calculate it.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}