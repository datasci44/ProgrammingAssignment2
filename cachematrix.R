## The following functions allow you to cache a matrix inversion.
## Matrix inversion can be a costly computation. Here we cache
## the inversion result to prevent unnecessary repetition.


## This function stores the matrix to be inverted. If cacheSolve
## is used to invert the matrix, the result is stored in the
## function environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is used in place of solve to invert a matrix
## called in the function makeCacheMatrix. If cacheSolve has
## previously inverted the matrix, a cached inversion will
## be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Example

# Store matrix object in makeCacheMatrix
# mat <- makeCacheMatrix(cbind(c(1,3,9), c(87,5,26), c(16,90,3)))

# First inverstion call using cacheSolve; computed result
# cacheSolve(mat)

# Second inversion call using cacheSolve; cached result
# cacheSolve(mat)
