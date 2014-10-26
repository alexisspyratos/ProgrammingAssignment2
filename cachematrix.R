## Put comments here that give an overall description of what your
## functions do

# Those two functions enable the user to cache any matrix inversion 
# that would already me made to avoid time comsumming calculation

## Write a short comment describing this function

# makeCacheMatrix creates an objects that will store the inverse of 
# the matrix if it has already been calculated

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

# cacheSolve enable to return directly the cached inversed of the matrix if
# the inverse has already been calculated

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