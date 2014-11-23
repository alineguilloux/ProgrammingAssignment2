## This functions work separetadly to set a matrix 
## and a list of functions associated with it and to store in cache
## the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # creates the object to store de inverse and sets it to null
  set <- function(y) { # defines function that allows the reset of matrix
    x <<- y # creates an object outside the functon environment to store new values for the matrix
    m <<- NULL # reset the inverse matrix to null when the original matrix is changed
  }
  get <- function() x ## defines an object that shows the actual values os the matrix
  setinv <- function(solve) m <<- solve ## creates a function to set m value that can be updated outside this function environment
  getinv <- function() m ## function that prints the value of the inverse matrix when m is set
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # create a list of results that can be called after the function has runned
}


## This function calculates and caches the value of the inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv() ## sets the value of m to the cached value - is null if not previously calculated
  if(!is.null(m)) { ## sets the use of cached data if m is not null.
    message("getting cached data") #defines a message to warn the data wasnt calculated again
    return(m) ## gives the output of the inverse matrix in cache
  }
  data <- x$get() ## Source the matrix if m was null
  m <- solve(data, ...) ## Calculates the inverse of the matrix and stores it in m
  x$setinv(m) ## sets the cache value of the inverse (as in the previous function)
  m ## prints the inverse
}