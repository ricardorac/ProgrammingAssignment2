## Following functions return inverse of a given invertible matrix. 
## Matrix inverion is perfomred if cached inverted matrix is unavailable.

## Creates a list of functions with the given matrix and allows caching the
## invert of this matrix.
makeCacheMatrix <- function(x = matrix()) {
  slv <- NULL
  set <- function(y) {
    x <<- y
    slv <<- NULL
  }
  get <- function() x
  setsolve <- function(s) slv <<- s
  getsolve <- function() slv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  
}


## This functions uses the return of makeCacheMatrix to calculate the inverse
## of a matrix and cache the inverse for future use.
## If the matirx is already solved and in the cache (parent environment),
## the inverted matrix is assigned to a local variable that is used by cacheSolve function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
