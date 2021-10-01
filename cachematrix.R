## Week 3 - Programming Assignment 2
## The two functions below will cache the inverse of a matrix.

## The first function, makeCacheMatrix, creates a special "matrix", which is really a 
## list containing a function to:
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse of the matrix using the solve() function
## 4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # Calculates the inverse of the matrix and caches the result
  setinverse <- function(solve) m <<- solve 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function, cacheSolve, calculates the inverse of the special "vector" created 
## with makeCacheMatrix. It will check first to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Else, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the solve() function.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}