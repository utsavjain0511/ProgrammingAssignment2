## This function returns the inverse of matrix. If the inverse of the matrix
## is calculated already the cached value is return. Once calculate it is cached.

## makeCacheMatrix returns the list of function on the object created 
## set -- sets the matrix
## get -- gets the set matrix
## getInverse -- gets the Inverse of matrix
## setInverse -- sets the Inverse of matrix in object inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function (y) {
    x <<- y 
    inverse <<- NULL 
  }
  get <- function() x 
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with
## the above function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
