## These functions check for a chached result of the inverse matrix and return
## that if not it is calculated

## This function creates a special matrix used to get the chached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(inv) m <<- inv
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## This funciton looks for cached data, if null performs calculation

cacheSolve <- function(x, ...) {
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
}
