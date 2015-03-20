## CacheMatrix: A faster way to get the inverse from a Matrix when 
## have already requested it
## makeCacheMatrix: this function creates an object with handlers
## to access a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversa <- function(inversa) i <<- inversa
  getinversa <- function() i
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}


## CacheSolve: This function calculates the inverse of a matrix. If it had been
## calculated before, it retrieves it from cache.

cacheSolve <- function(x, ...) {
  i <- x$getinversa()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversa(i)
  i
}
