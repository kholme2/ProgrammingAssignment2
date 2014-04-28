## The makeCacheMatrix function creates an object that holds
## a matrix and it's inverse. The cacheSolve function will
## return the inverse of the given matrix. If the given matrix's
## inverse is already in the cache object, the inverse is not
## recomputed.

## The makeCacheMatrix function takes a matrix and stores it's
## contents in an object wrapped with the set, get, setinverse,
## and getinverse functions. getinverse returns cacheMatrix which
## is initiallized to NULL and is set by the setinverse method.

makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) cacheMatrix <<- inverse
  getinverse <- function() cacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function returns the inverse of the given makeCacheMatrix
## object. If the matrix's inverse has been cahced, the inverse is returned
## with the getinverse method of makeCacheMatrix. If the inverse has not been
## cached, the inverse is computed with the solve() function and cached with
## the setinverse method of makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(matrix)
  inverse

}

