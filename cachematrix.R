#This function creates a matrix which can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of a square matrix returned by makeCacheMatrix above.
#If the inverse has already been calculated and the matrix is still the same,
#then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}