## Create function to define the matrix
makeCacheMatrix <- function(x = matrix()) {
  m  <- NULL
  
  ## set the value of the matrix
  set  <- function(y){
    x <<- y
    m <<- NULL 
  }
  
  ## get the value of the matrix
  get  <- function() x
  
  ## set the value of the inverse of the matrix
  setinverse  <- function(inverse) m  <<- inverse
  
  ## get the value of the inverse of the matrix
  getinverse  <- function() m
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

## The following function calculates the inverse of the matrix 
## created with the above function.

cacheSolve <- function(x, ...) {
  m  <- x$getinverse()
  
  ## Checks to see if the inverse has already been calculated (if the matrix has not changed)
  ## If so, it gets the inverse from the cache and skips the computation.
  
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## calculates the inverse of the data using solve() and sets the value of 
  ## the inverse in the cache via the setinverse function
  
  data  <- x$get()
  m  <- solve(data, ...)
  x$setinverse(m)
  m
}