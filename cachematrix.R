## These functions help cache the inverse of a matrix to avoid redundant calculations. 
## `makeCacheMatrix` creates a special object to store the matrix and its inverse. 
## `cacheSolve` computes the inverse, using the cached value if it's already available.

## Creates a special matrix object to store a matrix and its cached inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## m stores the cached inverse matrix
  m <- NULL
  
  ## Set a new matrix and clear the cached inverse
  set <- function(y) {
    x <<- y      # Save the matrix
    m <<- NULL   # Reset the cached inverse
  }
  
  ## Get the current matrix
  get <- function() x
  
  ## Set the cached inverse
  setInverse <- function(inverse) m <<- inverse
  
  ## Get the cached inverse
  getInverse <- function() m
  
  ## Return the list of functions for matrix and inverse access
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the matrix, using the cache if it's available
cacheSolve <- function(x, ...) {
  
  ## Check if the inverse is already cached
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If not cached, compute the inverse
  mat <- x$get()  # Get the matrix
  m <- solve(mat, ...)  # Calculate the inverse
  
  ## Cache the inverse for future use
  x$setInverse(m)
  
  ## Return the computed inverse
  m
}
