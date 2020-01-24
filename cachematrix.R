## Put comments here that give an overall description of what your
## functions do

## In this code we complete two functions makeCacheMatrix and cacheSolve to catch the matrix inverse


## The function below (makeCacheMatrix) defined to create a special matrix object that 
## catches its iverse for the input


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function (cacheSolve) tends to compute the inverse of the previously created special
## matrix. If we already have the invese calculated for the same matrix, then the cachesolve function 
## will retreive the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
