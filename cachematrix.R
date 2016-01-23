## Create makeCacheMatrix - a function which creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The function cacheSolve returns the inverse of the matrix. It first checks if the inverse has already been computed. 
#If so, it gets the result and skips the computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is a square matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample run:
## > x = matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4

## No cache in the first run
## > cacheSolve(m)
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           getting cached data.
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

