#The following functions are used to create a special object that stores a matrix and caches its inverse. 
#The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
  
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invr <<- inverse
  getInverse <- function() invr
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invr <- x$getInverse()
  if (!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  matx <- x$get()
  invr <- solve(matx, ...)
  x$setInverse(invr)
  invr
}

