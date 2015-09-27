## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of inv matrix
  setinv <- function(inv) i <<- inv
  
  ## Get the value of inv matrix
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
  ## Check if the inverse matrix had been calculated (if  yes return inverse matrix i)
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## In case inverse is not calculated get the matrix, calculate it inverse using solve
  data <- x$get()
  i <- solve (data, ...)
  x$setinv(i)
  ## Return a matrix that is the inverse of 'x'
  i
}