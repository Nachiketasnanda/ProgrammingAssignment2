## Put comments here that give an overall description of what your
## functions do

## Objective: To write a pair of functions to cache the inverse of the matrix
## Write a short comment describing this function

## Function makeCacheMatrix
# Create a special matrix that will cache the inverse of the matrix

makeCacheMatrix <- function(mx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
}
## Write a short comment describing this function
## Fuction cacheSolve to caculate the inverse of the special matrix. 
## Checks if the inverse has been already calculated and the matrix has not changed, then
## cacheSolve would return inverse from the cache

cacheSolve <- function(mx, ...) {
  inverse <- mx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mx$get()
  invserse <- solve(data, ...)
  mx$setinv(inverse)
  return(inverse)
}

