## Author: Gustavo
## Date: 04.26.2014

## makeCacheMatrix stores the inverse object
makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  ## sets the matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  ## return the matrix object
  get <- function() x
  
  ## sets inverse 
  setinverse<- function(i) inverse <<- i
  
  ## returns inverse
  getinverse <- function() inverse
  
  ## return all of the functions in this function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
   
}


## If matrix inverse not cached, then calculate and cache it
## At the end, return inverse

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  
  ## get inverse of x
  inverse <- x$getinverse()
  
  ## if it is already cached, then return inverse
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  ## If the inverse is not cached, then get the matrix x from object
  data <- x$get()
  
  ## Calculate inverse & set inverse into object
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  ## return inverse
  inverse
}
