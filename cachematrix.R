## Calculating matrix inversions can be a costly operation. Thus the
## functions in this file cache a matrix and its inversion in a lazy
## manner.  The inversion is calculated only when needed, and then it
## is cached so it can be retrieved without recalculation unless/until
## the matrix is changed.

## This function is a list object that provides setters and getters
## for storing a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- x
  # Test for NULL input and stop processing if found
  if (is.null(mat)) {
    stop('Input is NULL.')
  }
  
  # Test whether input is a matrix; attempt coercion if not
  if (!is.matrix(mat)) {
    warning('Input is not a matrix. Attempting coercion')
    mat <- as.matrix(mat)
  }
  
  # When the makeCacheMatrix is called, the inverse is set to NULL
  inv <- NULL
  
  setMatrix <- function(y) {
    if (!isSameMatrix(mat,y)) {
      mat <<- y
      inv <<- NULL
    }
  }
  
  getMatrix <- function() {
    mat
  }
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  getInverse <- function() {
    inv
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of the provided matrix. If the 
## inverse is already cached, it returns the cached inverse.  Otherwise
## it calculates the inverse, caches the inverse, then returns it to the 
## caller.

cacheSolve <- function(x, ...) {
  mat <- x
  # Retrieve the inverse
  inv <- mat$getInverse()
  # If the inverse is already cached, simply return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If the inverse is not already cached, calculate, store and return it
  data <- mat$getMatrix()
  inv <- solve(data, ...)
  mat$setInverse(inv)
  inv
}

# This utility function tests whether two matrices are the same
isSameMatrix <- function(x, y) {
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}

# This function runs tests of the others
matrixTest <- function (inputMatrix) {
  
  testCacheMatrix <- makeCacheMatrix(inputMatrix)
  
  print('Test: Create new cached matrix; inversion is NULL')
  print('Matrix')
  print(testCacheMatrix$getMatrix())
  print('Inversion')
  print(testCacheMatrix$getInverse())
  
  print('Test: Calculate inversion of cached matrix; inversion is populated')
  cacheSolve(testCacheMatrix)
  print('Matrix')
  print(testCacheMatrix$getMatrix())
  print('Inversion')
  print(testCacheMatrix$getInverse())
  
  print('Test: Update cached matrix, but an idential matrix; inversion is already cached')
  testCacheMatrix$setMatrix(inputMatrix)
  cacheSolve(testCacheMatrix)
  print('Matrix')
  print(testCacheMatrix$getMatrix())
  print('Inversion')
  print(testCacheMatrix$getInverse())
  
  print('Test: Update cached matrix with new matrix; inversion is NULL')
  testCacheMatrix$setMatrix(testCacheMatrix$getMatrix()+testCacheMatrix$getMatrix())
  print('Matrix')
  print(testCacheMatrix$getMatrix())
  print('Inversion')
  print(testCacheMatrix$getInverse())
  
  print('Test: Retrieve inversion; inversion is calculated anew')
  cacheSolve(testCacheMatrix)
  print('Matrix')
  print(testCacheMatrix$getMatrix())
  print('Inversion')
  print(testCacheMatrix$getInverse())

  print('Test: Retrieve inversion; inversion is cached')
  cacheSolve(testCacheMatrix)
  print('Matrix')
  print(testCacheMatrix$getMatrix())
  print('Inversion')
  print(testCacheMatrix$getInverse())  
}
