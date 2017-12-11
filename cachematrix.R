## The following two functions together find the inverse of a matrix.
## For efficiency, it will use the cached inverse of the matrix if
## it exists; otherwise it will compute the inverse from scratch.

## The makeCacheMatrix function creates a special vector that
## sets the value of the matrix
## gets the value of the matrix
## sets the inverse of the matrix
## gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inversematrix) inv <<- inversematrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The cacheSolve function calculates the inverse of the matrix with
## with the above makeCacheMatrix fuction. If the inverse matrix
## already exists it will use the cache value, otherwise is will
## calculated the inverse matrix using the solve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
