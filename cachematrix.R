## The file contains functions to store a matrix together with its inverse.
## This can be useful, if the inverse is needed more often and the computation of the 
## inverse is intensive.
## Use makeCacheMatrix to create the object, makeCacheMatrix$get to get the matrix 
## and cacheSolve to get the inverse of the matrix.



## Creates an object (list) which stores a matrix x and later maybe the inverse of x.
## The list contains of four functions:
## - get: returns the stored matrix
## - set: set a matrix into the object (the inverse will be reseted)
## - getInv: returns the inverse matrix
## - setInv: set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(matInv = matrix()) inv <<- matInv
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Return a matrix that is the inverse of 'x', for x created by makeCacheMatrix
## If x already stores the inverse, this value is returned.
## Else the inverse of the matrix is computed (using solve), stored as inverse in x 
## and returned.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    ## message("using cached matrix")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}