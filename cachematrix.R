## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function is exactly like the makeVector function
## in the example but instead of mean we store the inverse
## getinverse & setinverse functions sets and gets the inverse

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve, as in the example, computes the inverse(in the example the mean) 
## only if it wasn't previously computed.
cacheSolve <- function(x, ...) {

  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  inverse    
}
