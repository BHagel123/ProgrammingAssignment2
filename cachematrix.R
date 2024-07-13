## makeCacheMatrix and cacheSolve can be used to return the inverse of a matrix.
## If the inverse of the matrix is already stored in memory, it will return that 
## value along with the message 'getting cached data'. This prevents recalculation 
## of the inverse of the matrix. Lexical scoping allows cacheSolve to access the
## environment from makeCacheMatrix (functions and data)

## makeCacheMatrix creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve will calculate and store the inverse of the matrix from 
## makeCacheMatrix, but first checks to see if the inverse of the matrix has 
## already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
