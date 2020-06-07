## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions, makeCacheMatrix and cacheSolve, are used in 
## conjunction to cache the inverse of a matrix.

## The assignment assumes that the matrix supplied is always invertible, so 
## there is no error handling developed for matrices that have no inverse

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  # set the value of a matrix
  set <- function(y) {
    x <<- y
    # restores to null the value of the inverse v, because the old
    # inverse of the old matrix is not needed anymore
    v <<- NULL
  }
  
  # get the value of a matrix
  get <- function() x
  # set the value of the inverse
  setinverse <- function(inverse) v <<- inverse 
  # get the value of the inverse
  getinverse <- function() v
  
  # setting up the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)   
}

## This function computes the inverse of the special "matrix" returned  by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed, then cacheSolve should retrieve the inverse from 
## the cache)

cacheSolve <- function(x, ...) {
  
  v <- x$getinverse() # x$v
  
  # First checks to see if the inverse has already been calculated; if so, 
  # it gets the inverse from the cache and skips the computation
  if (!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  
  # Otherwise, it calculates the inverse of the data and sets the value of 
  # the inverse in the cache via the setinverse function
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
}