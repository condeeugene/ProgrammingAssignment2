Conde <- function( q = matrix() ) {
  inverted <- NULL
  set <- function( matrix ) {
    q <<- matrix
    inverted <<- NULL
  }
  get <- function() {
    q
  }
  setInverse <- function(inverse) {
    inverted <<- inverse
  }
  getInverse <- function() {
    inverted
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  q <- x$getInverse()
  if( !is.null(q) ) {
    message("getting cached data")
    return(q)
  }
  data <- x$get()
  q <- solve(data) %*% data
  x$setInverse(q)
  q
}