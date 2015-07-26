## Two functions for handling matrices are contained in this file
## The aim is to inverting matrices in an efficient way
##
## The first function will construct a special matrix
## The second function will use the first matrix and
## store a cached version of its inverse in it

## This function takes a matrix and stores it internally.
## It contains getters and setters which can be invoked externally
## Also, it can store (and cache) its own inverse
## so there is no need to calculate over and over
##
## parameters: x matrix()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function "solves" a matrix by calculating its
## inverse. The matrix must be constructed by the function
## makeCacheMatrix described above.
## It first checks whether there is already a stored
## version of the inverse, and if not it will be calculated
## and returned
## in any case the inverse will be returned (if invertible)
##
## Preconditions: Matrix must be invertible
##
## Paramters x Matrix from makeCacheMatrix described above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}