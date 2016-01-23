## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invXmat <- x$getInverse()
  if (!is.null(invXmat)) {
    message("getting cached data")
    return(invXmat)
  }
  mat <- x$get()
  invXmat <- solve(mat, ...)
  x$setInverse(invXmat)
  invXmat
}
#testing

my_matrix <- makeCacheMatrix(matrix(10:13, 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$set(matrix(c(2, 5, 7, 3), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
commit