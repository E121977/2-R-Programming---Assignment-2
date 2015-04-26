makeCacheMatrix <- function(x = matrix()) {
  Im <- NULL
  set <- function(y) {
    x <<- y
    Im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Im <<- inverse
  getinverse <- function() Im
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
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