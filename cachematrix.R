## cachematrix.r
## Provides two functions, makeCacheMatrix and cacheSolve
##
## makeCacheMatrix: creates a special matrix, which is basically a list of functions that:
## 1 - set the values of the matrix
## 2 - get the values of the matrix
## 3 - set the values of the inverse matrix
## 4 - get the values of the inverse matrix
##
## cacheSolve: calculates the inverse of the special matrix created by the previous function
## It first checks if the inverse has already been calculated, and if so, gets the inverse
## from the "cache" and skips computation. Otherwise it will calculate the inverse and set
## the inverse in the cache via the setinverse function

## Creates the special matrix object
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse if it does not exist, otherwise retrieves the inverse matrix from the cache
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
