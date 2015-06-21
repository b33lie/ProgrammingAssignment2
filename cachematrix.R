## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This  makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix
makeCacheMatrix <- function(inputVal = matrix()) {
  inverse <- NULL
  set <- function(x) {
    inputVal <<- x;
    inverse <<- NULL;
  }
  get <- function() return(inputVal);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(
        set = set, 
        get = get, 
        setinv = setinv, 
        getinv = getinv ))
}

## This cacheSolve function computes 
## the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(inputVal, ...) {
  inverse <- inputVal$getinv()
  
  if(!is.null(inverse)) {
    message("getting cached data...")
    return(inverse)
  }
  
  data <- inputVal$get()
  inverse <- solve(data, ...)
  inputVal$setinv(inverse)
  
  return(inverse)
}

################
## Test run   ##
################

# > x <- matrix(rnorm(9), nrow = 3)           // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, same value with above
#    