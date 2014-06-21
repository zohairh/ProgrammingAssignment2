## We will make a special matrix object called CacheMatrix, which will cache the inverse of the matrix within itself
## and contain a list of functions to get and set the matrix and the inverse

## This function creates a CacheMatrix object and optionally initializes the matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL    
  }
  get <- function() x
  getinv <- function() invm
  setinv <- function(i) {
    invm <<- i
  }
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## this function calculates the inverse of the CacheMatrix unless the value is already chached,
## in which case it simply returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (is.null(i)) {
    m <- x$get()
    i<-solve(m, ...)
    x$setinv(i)
  } else {
    message("getting cached inverse")
  }
  i
}
