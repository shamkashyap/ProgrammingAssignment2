## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a matrix and sets its inverse (inv) to NULL
## It creates a list of 4 functions as well (get and set, getInverse, setInverse)

makeCacheMatrix <- function(x = matrix()) {
  #mark the inverse as null
  inv <- NULL
  set <- function(y) {
    #Using <<- since the x we want is not in the scope of this function
    x <<- y 
    #if someone resets the matrix, inverse becomes null, so that it can be calculated afresh
    inv <<- NULL 
  }
  #returns the matrix
  get <- function() x
  setInverse <- function(invMat) inv <<- invMat
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function tries to see if the inverse matrix is already present
## If it ins't, it calculates the inverse using solve.
## If it is already present, returns it without calculating inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
