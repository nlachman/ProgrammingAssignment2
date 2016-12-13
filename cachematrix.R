## Programming Assignment 2


## This function creates a "special" matrix that caches its inverse
makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                           ## initialize inv as NULL
  set <- function(y) {                  ## define the set function to assign new 
    x <<- y                           ## matrix value in parent environment
    inv <<- NULL}                     ## if there is a new matrix, reset inv to NULL
  get <- function() x                   ## returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  ## this allows you to use $ operator in the cacheSolve function
}

## This function computes the inverse of the matrix returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)}
  sampledata <- x$get()
  inv <- solve(sampledata, ...)
  x$setinverse(inv)
  inv}