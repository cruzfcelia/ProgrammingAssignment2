## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
      ## makeCacheMatrix: This function creates a special "matrix" object that can 
      ## cache its inverse.
      ## x is an invertible matrix
      ## returns a list of four functions:
      ## set: set the matrix
      ## get: get the matrix
      ## setInverse: set the inverse
      ## getInverse: get the inverse
      
      inv <- NULL

      # set the matrix
      set <- function(y) {
            # `<<-` assigns a value to an object in an environment 
            # different from the current environment. 
            x <<- y
            inv <<- NULL
      }
      
      # get the matrix
      get <- function() x
      
      # set the inverse
      setInverse <- function(inverse) inv <<- inverse
      
      #get the inverse
      getInverse <- function() inv
      
      #list of functions
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
      ## cacheSolve: This function computes the inverse of the special "matrix"(x) 
      ## returned by makeCacheMatrix to retreive its inverse, either using 
      ## pre-existing stored value, or by calculating it, and caching it on the 
      ## provided matrix (x).
      ## x is the output of makeCacheMatrix()
      ## returns the inverse of the original matrix input to makeCacheMatrix()
      
      # In the case the inverse has already been calculated, returns from cache
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # In the case inverse has not been calculated, calculates the inverse
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv) #sets the inverse in the cache
      inv
}
