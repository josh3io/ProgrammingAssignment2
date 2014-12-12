## these functions work together to cache the inverse of a matrix for future use
## without need to recalculate the solution.
## assumes all input matrices are solvable
##
## e.g.:
##
## > a <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2))
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(a)
## cached
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## create a CacheMatrix "object" with an input matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## with CacheMatrix "object" return the inverse.
## if the inverse is already computed, return the cached value.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("cached")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
