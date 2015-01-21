## These functions are designed to cache the inverse of a matrix.


# This function creates a special matrix object.
makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ix <<- inverse
  getinverse <- function() ix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


# This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
# If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ix <- x$getinverse()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  # Assuming data is invertible...
  ix <- solve(data, ...) 
  x$setinverse(ix)
  ix
}