## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # this is for initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get <- function(){x} # Function to get matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function(){ # Function to get inverse of the matrix
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}
#This is used for get the cache data
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) { #checks if inverse is null
    message("getting cached data")
    return(inv) #returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv # returns the inverse matrix of x
}