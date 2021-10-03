## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

Conde <- function( q = matrix() ) {
  
  ## Initialize the inverse property
  
  inverted <- NULL
  
  ## Method to set the matrix
  
  set <- function( matrix ) {
    q <<- matrix
    inverted <<- NULL
  }
  
  ## Method the get the matrix
  
  get <- function() {
    
  ## Return the matrix  
    
    q
  }
  
  ## Method to set the inverse of the matrix
  
  setInverse <- function(inverse) {
    inverted <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  
  getInverse <- function() {
    
  ## Return the inverse property
    
    inverted
  }
  
  ## Return a list of the methods
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  q <- x$getInverse()
  
  ## Just return the inverse if its already set
  
  if( !is.null(q) ) {
    
    message("getting cached data")
    return(q)
  }
  ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  q <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(q)
  ## Return the matrix
  q
}