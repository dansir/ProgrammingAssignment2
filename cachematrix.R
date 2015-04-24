## CacheInverse - these functions can cache the inverse of a matrix


## This function creates a vector that can set and get the value of the 
## vector and inverse of the vector that is inputted

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) s <<- solve
  getInverse <- function() s
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Calculates the matrix that is the inverse of matrix inputted;
## First checks if it has already been cached

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
