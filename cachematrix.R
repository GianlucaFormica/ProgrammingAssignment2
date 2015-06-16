## makeCacheMatrix: This function creates a special "matrix" object 
##  that can cache its inverse.

## cacheSolve: This function computes the inverse of the 
##  special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), 
##     then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set is a function that changes the vector stored in the main function.
  ## "x <<- y" substitutes the vector x with y (the input) in the main function (makeVector)
  set <- function(y) {
    x <<- y
    ## "inv <<- NULL" restores to null the value of the inverse matrix inv, 
    ##  because the old mean of the old vector is not needed anymore. 
    ##   The new mean needs to be recalculated through the function cachemean.
    inv <<- NULL
  }
  ## get is a function that returns the vector x stored 
  ##  in the main function. Doesn't require any input.
  get <- function() x
  ## setinv and getinv are functions very similar to set and get. 
  ##  They don't calculate the inverse matrix, 
  ##   they simply store the value of the input in a variable inv into the main function makeVector (setinv) and return it (getinv).
  ##    The value "inverse", input of seatinv, is supposed to be the inverse of the vector x. However it simply stores a value
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  ## To store the 4 functions in the function makeVector,
  ##  we need the function list(), so that when we assign makeVector to an object, 
  ##   the object has 4 functions: set, get, setinv, getinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##  The first thing cachemean does is to verify the value inv, 
  ##   stored previously with getinv, exists and is not NULL. 
  ##    If it exists in memory, it simply returns a message and the value inv, 
  ##      that is supposed to be the inverse, but not necessarily.
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##  everything that follows this if() is a sort of else {}. 
  ##    data gets the vector stored with makeVector, inv calculates the inverse of the 
  ##      vector using the function solve(), and x$setinv(inv) stores it in the object 
  ##        generated assigned with makeVector.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
