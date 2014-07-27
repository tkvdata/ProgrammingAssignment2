## These functions create a newly defined object a makeCacheMatrix, that takes a matrix
## its input.  This object can cache the value of the inverse as one of its properties.
## Once stored the object does not need to recalculate the inverse - it can
## just retrieve the data stored in "inverse"

## makeCacheMatrix initializes a makeCacheMatrix object with a matrix input and some help
## functions such as setinverse, getinverse, set, and get which in turn can be used to
## retrieve or set properties of makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseM) inverse <<- inverseM
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

}


## cacheSolve checks if the makeCacheMatrix object that has been created has
## already stored the value of the inverse.  If it has, it simply uses that that value
## retrieved from the getinverse method (which is public)

## if it has yet to be stored as a property of the object, cacheSolve grabs the matrix
## data using the public method get, and then solves for the inverse and sets it as a 
## property of the makeCacheMatrix object

cacheSolve <- function(x, ...) {
  ## get inverse using public method created in makeCacheMatrix
  inverse <- x$getinverse()
  ## if it has a previously stored inverse - retrieved instead of recalculating
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## if no previously stored data solves for the inverse of the matrix data and sets
  ## it as a property of the makeCacheMatrix object
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
