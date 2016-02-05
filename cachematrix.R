## This function creates a special "matrix", 
# which is really a list containing a function to:
# set, get the value of the matrix
# set, get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y #caches the inputted matrix
    inv <<- NULL #sets the value of inverse
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## This function computes the inverse of the "matrix" 
# returned by makeCacheMatrix.If the inverse has already been 
# calculated, then skip calculation and get data from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # gets inverse from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # if inv<>0 (has already been calculated)
    #get it from cache
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv) #if inv=0, then calculate it and set
  # in the cache 
  inv
}
