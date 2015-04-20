# cachematrix
R Programming Assignment 2

## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
##inverse of a matrix rather than computing it repeatedly.
##The two functions below are a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix", which is really a matrix containing a function to
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse of the matrix
#4.get the value of the inverse of the matrix

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
m<-matrix(c(-1,-2,1,1), 2:2)
u<-makeCacheMatrix(m)
as.list(environment(u$get))
u$getinv()

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
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
cacheSolve(u)
