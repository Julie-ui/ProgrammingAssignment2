## makeCacheMatrix(): argument takes an INVERTIBLE matrix 
## RETURNS: a list to the parent environment enclosing all the defined functions (set, get, setInverse, getInverse) and the data objects x and inverse.matrix. They can be then used to feed in the caheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL 
  set <- function(y) { 
    x <<- y 
    inverse.matrix <<- NULL 
  }
  get <- function() x 
  setInverse <- function() inverse.matrix <<- solve(x) 
  getInverse <- function() inverse.matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve(): is used to retrive the cached value of the inverse.matrix from an object returned by makeCachMatrix(). This function checks wether the cached inverse.matrix is equal to NULL. If this is not the case, we have a valid inverse.matrix which is returned to the parent environmetnt. Else, if the cahced inverse.matrix is equal to NULL, the function gets the original matrix from the inputted makeCacheMatrix() object and calculates the inverse matrix.
## RETURNS: The inverse of the matrix

cacheSolve <- function(x, ...) {
  inverse.matrix <- x$getInverse()
  if(!is.null(inverse.matrix)) {
    message("getting cached data")
    return(inverse.matrix)
  }
  data <- x$get()
  inverse.matrix <- solve(data, ...)
  x$setInverse()
  inverse.matrix
}

# EXAMPLE:
# my.matrix <- matrix(data = c(rep(1, 3), 2, 1, 4, 1:3), nrow = 3, ncol = 3) 
# m <- makeCacheMatrix(my.matrix)
# cacheSolve(m)
