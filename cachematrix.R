## Programming Assignment 2
## creating a cached matrix inversion to eliminate recalculating

## Creates a cached matrix to eliminate recalculation inverse
## stores the matrix and its inverse if calculated already

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m<<-inverse
  getInverse <- function() m
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## if the inverse has been calculated, return it without solving again
## if not then solve and store result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if( !is.null(m)) {
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
