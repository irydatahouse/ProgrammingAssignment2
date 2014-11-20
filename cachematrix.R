##  These are R functions are able to cache
##  potentially time-consuming computations.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  checkinverse <- function(){
    if (is.null(inv)) {
      message("can't check inverse  - inverted matrix is NULL")      
      return()
    }
    
    inv %*% x
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       checkinverse = checkinverse)

}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. 
## If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve
##  should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data (inverted matrix)")
    return(inv)
  }
  data <- x$get()
  nc <- ncol(data)
  nr <- nrow(data)

  if(nc != nr) {
    message("non-square matrix - inverse is NULL")
  } else {
    inv <- try(solve(data,...))
  }
  
  x$setinverse(inv)
  
  inv

}
