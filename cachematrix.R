## Put comments here that give an overall description of what your
## functions do
## This function creates a special matrix, can cach its inverse. 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
  x <<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(solve) m <<- solve
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## Write a short comment describing this function
## Computes the inverse of the special matrix, 
## returned by makeCacheMatrix. If the inverse hase already been
## calculated and the matrix has not changed then the cachesolve
## should retrieve the inverse from the cache. If the inverse has 
## not been calculated, data gets the matrix stored with makeCachMatrix
## m calculates the inverse. x$setmean(m) stores in m in makeCachMatrix

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
