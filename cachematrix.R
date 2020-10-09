## The functions below will create a matrix and compute its inverse.
## If the inverse computation is repeated, a cached version of the inverse
## matrix will be returned.

## This function will create a matrix that can cache its inverse

makeMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function will take the inverse of a matrix and cache it if its cache
## does not already exist. If it exists, it returns it.

cacheSolve <- function(x) {
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("Getting cached Matrix...")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverseMatrix(m)
  m
}
