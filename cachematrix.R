## A pair of functions that cache the inverse of a matrix

## Creates a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      ## Get the original matrix
      get <- function() x
      ## Set the original inverse and reset the inverse
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      ## Get inverse value
      getinv <- function() inv
      ## Set inverse value
      setinv <- function(inverse) inv <<- inverse
      ## Returns a list of the 4 functions
      list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Calculate the inverse of the matrix returned by makeCacheMatrix, if previously calculated, returns cached value

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      
      # return cached matrix inverse
      if (!is.null(inv)) {
            message("Getting cached inverse")
            return(inv)
      }
      
      # calculate inverse of matrix 
      m <- x$get()
      inv <- solve(m)
      
      # cache inverse
      x$setinv(inv)
      
      # return matrix inverse
      inv
}
