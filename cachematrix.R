# To save on CPU cycles, 
# the makeCacheMatrix and cacheSolve functions
# provide a caching utility for storing and retrieving
# a matirx along with its inverse. 
#
# 
#
# The makeCacheMatrix function creates a list object with functions to:
#  > set the value of the matrix
#  > get the value of the matrix
#  > set the value of the matrix inverse
#  > get the value of the matrix inverse
# This function offers no error handling; it assumes user will provide
# a square, non-singular matrix, and a valid inverse 
# (if not set via cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The cacheSolve function computes, caches, and returns
# the inverse of a matrix cached by the makeCacheMatrix function. 
# If the inverse has already been calculated 
# (and the matrix has not changed), 
# cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# # Usage Example:

# # Create a 3x3 matrix from random, normal data
# dat <- matrix(rnorm(9,80,5), 3, 3, T)

# # initiate matrix container
# mat <- makeCacheMatrix()

# # store matrix in container
# mat$set(dat)

# # validate it worked by retrieving stored matrix
# mat$get()

# # generate inverse
# cacheSolve(mat)

# # check for identiy matrix
# mat$get() %*% mat$getinverse() 
