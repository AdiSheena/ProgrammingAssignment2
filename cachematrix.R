## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  INVERSE <- NULL
  set <- function(y) {
    x <<- y
    INVERSE <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) INVERSE <<- inverse
  getinverse <- function() INVERSE
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  INVERSE <- x$getinverse()
  if(!is.null(INVERSE)) {
    message("getting cached data")
    return(INVERSE)
  }
  data <- x$get()
  INVERSE <- inverse(data, ...)
  x$setinverse(INVERSE)
  INVERSE
}

