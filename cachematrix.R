## Creates an object, contains a matrix data structure and its inverse
##  get = gets the value of the matrix
##  set = changes the value of the matrix
##  getInverse = gets the cached inverse of the matrix once set
##  setInverse = sets the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  Im <- NULL
  set <- function(y) {
    x <<- y
    Im <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) Im <<- solve
  getInverse <- function() Im
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

## returns the cached inverse value of cachematrix if available
## otherwise calculates, caches, and returns the inverse of the cachematrix

cacheSolve <- function(x=matrix(), ...) {
  Im <- x$getInverse()
  if(!is.null(Im)) {
    message("getting cached data")
    return(Im)
  }
  data <- x$get()
  Im <- solve(data, ...)
  x$setInverse(Im)
  Im
}