## We have 2 functions here makeCacheMatrix and cacheSolve. 
## The makeCacheMatrix is mainly used as a cache. 
## The get and set methods are used to set and get default values 
## while, setInverse and getInverse are used to set and get the inverse of matrix
## cacheSolve first makes a check to see if the inverse of matrix is already
## stored or not. If not, then it calculates and save the inverse
## to the cache


## Creates cacheable matrix for inputting to
## cacheSolve() function which sets and gets 
## the cached values


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) 
  x$setInverse(i)
  i
}

