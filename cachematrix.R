## Calculates the inverse of a square matrix, and cache the result 
## so that it doesn't have to be computed again


## makeCacheMatrix: Creates a special matrix that cache its inverse
## NOTE: the matrix must be a "square" matrix!!

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse matrix of the object created by 'makeCacheMatrix'
## If the inverse has already been calculated, return the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i ## Return a matrix that is the inverse of 'x'
}

## Example:

M3 <- matrix(rnorm(25^2), 25, 25)
M4 <- makeCacheMatrix(M3)

cacheSolve(M4)
cacheSolve(M4) 

## as the function already calculated the inverse matrix, return 
## the cached object

