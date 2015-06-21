## The functions are use to generate an inverse for a given matrix
## If the inverse was already calculated returns the cached result
## If the inverse was not yet calculated, it calculates the inverse 
## and caches the result for further use

## This function is used to encapsulate the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function is used to return the inverse of the matrix if 
## it exists or it calculates and caches the inverse if it doesn't exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,diag(nrow(data)))
  x$setInverse(i)
  i
}
