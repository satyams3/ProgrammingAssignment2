makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  
  set <- function(y) {
    x <<- y
    
    v <<- NULL
  }
  

  get <- function() x
  
  setinverse <- function(inverse) v <<- inverse 
 
  getinverse <- function() v
  
 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)   
}



cacheSolve <- function(x, ...) {
  
  v <- x$getinverse() 
  
  
  if (!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  

  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
}