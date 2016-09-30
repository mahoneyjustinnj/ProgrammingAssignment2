## A matrix inversion is usually a costly computation
## There is benefit to caching the inverse of a matrix   
## rather than computing it repeatedly

## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse)m<<-inverse
  getInverse <- function()m
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
  }
  
## Compute the inverse of the matrix returned
## by makeCacheMatrix(), unless the inverse has
## already been calculated. In this case,
## it retrieves it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() 
  if(!is.null(m)){
    message ("getting cached data")
    return(m)
  }
  m<-solve(x$get())
  x$setInverse(m)
  m
}

