## This function Caches the inverse of a matrice in order to avoid long
## computation of matrix inverse. The calculated amount will be used when needed.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
  
}
